;; Energy Grid Trading System
;; Peer-to-peer energy trading platform with grid balancing and automated settlements

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-input (err u102))
(define-constant err-insufficient-energy (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-trade-expired (err u105))
(define-constant err-grid-unstable (err u106))

;; Data variables
(define-data-var trade-counter uint u0)
(define-data-var grid-operator principal contract-owner)
(define-data-var grid-stability-threshold uint u8000) ;; 80% stability minimum
(define-data-var current-grid-load uint u5000) ;; Current grid load percentage

;; Energy source types
(define-constant source-solar "SOLAR")
(define-constant source-wind "WIND")
(define-constant source-hydro "HYDRO")
(define-constant source-battery "BATTERY")
(define-constant source-grid "GRID")

;; Trade status
(define-constant status-pending "PENDING")
(define-constant status-active "ACTIVE")
(define-constant status-completed "COMPLETED")
(define-constant status-cancelled "CANCELLED")

;; Energy producers registry
(define-map energy-producers
  { producer: principal }
  {
    energy-source: (string-ascii 20),
    capacity-kwh: uint,
    available-energy: uint,
    price-per-kwh: uint,
    location-grid: (string-ascii 10),
    certified-renewable: bool,
    active: bool,
    total-trades: uint,
    reputation-score: uint
  }
)

;; Energy consumers registry
(define-map energy-consumers
  { consumer: principal }
  {
    max-demand-kwh: uint,
    preferred-sources: (list 5 (string-ascii 20)),
    location-grid: (string-ascii 10),
    green-preference: bool,
    active: bool,
    total-purchases: uint,
    average-rating: uint
  }
)

;; Energy trades
(define-map energy-trades
  { trade-id: uint }
  {
    producer: principal,
    consumer: principal,
    energy-amount: uint,
    price-per-kwh: uint,
    total-price: uint,
    energy-source: (string-ascii 20),
    delivery-time: uint,
    expiry-time: uint,
    status: (string-ascii 20),
    grid-impact: int,
    created-at: uint,
    completed-at: (optional uint)
  }
)

;; Grid balancing records
(define-map grid-balancing
  { timestamp: uint }
  {
    total-supply: uint,
    total-demand: uint,
    grid-stability: uint,
    renewable-percentage: uint,
    peak-demand: bool,
    balancing-actions: (list 10 (string-ascii 50))
  }
)

;; Energy certificates (for renewable energy)
(define-map energy-certificates
  { cert-id: uint }
  {
    producer: principal,
    energy-source: (string-ascii 20),
    energy-amount: uint,
    certification-body: (string-ascii 50),
    issued-at: uint,
    valid-until: uint,
    carbon-offset: uint
  }
)

;; Pricing data
(define-map energy-pricing
  { grid-location: (string-ascii 10) }
  {
    base-price: uint,
    peak-multiplier: uint,
    renewable-premium: uint,
    last-updated: uint
  }
)

;; Authorization functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-grid-operator)
  (is-eq tx-sender (var-get grid-operator))
)

(define-private (is-registered-producer (producer principal))
  (match (map-get? energy-producers { producer: producer })
    prod (get active prod)
    false
  )
)

(define-private (is-registered-consumer (consumer principal))
  (match (map-get? energy-consumers { consumer: consumer })
    cons (get active cons)
    false
  )
)

;; Producer registration and management
(define-public (register-producer
  (energy-source (string-ascii 20))
  (capacity-kwh uint)
  (price-per-kwh uint)
  (location-grid (string-ascii 10))
  (certified-renewable bool)
)
  (begin
    (asserts! (> capacity-kwh u0) err-invalid-input)
    (asserts! (> price-per-kwh u0) err-invalid-input)
    
    (map-set energy-producers
      { producer: tx-sender }
      {
        energy-source: energy-source,
        capacity-kwh: capacity-kwh,
        available-energy: capacity-kwh,
        price-per-kwh: price-per-kwh,
        location-grid: location-grid,
        certified-renewable: certified-renewable,
        active: true,
        total-trades: u0,
        reputation-score: u5000
      }
    )
    (ok true)
  )
)

(define-public (update-available-energy (new-available uint))
  (begin
    (asserts! (is-registered-producer tx-sender) err-unauthorized)
    
    (match (map-get? energy-producers { producer: tx-sender })
      producer
      (begin
        (asserts! (<= new-available (get capacity-kwh producer)) err-invalid-input)
        (map-set energy-producers
          { producer: tx-sender }
          (merge producer { available-energy: new-available })
        )
        (ok true)
      )
      err-not-found
    )
  )
)

;; Consumer registration
(define-public (register-consumer
  (max-demand-kwh uint)
  (preferred-sources (list 5 (string-ascii 20)))
  (location-grid (string-ascii 10))
  (green-preference bool)
)
  (begin
    (asserts! (> max-demand-kwh u0) err-invalid-input)
    
    (map-set energy-consumers
      { consumer: tx-sender }
      {
        max-demand-kwh: max-demand-kwh,
        preferred-sources: preferred-sources,
        location-grid: location-grid,
        green-preference: green-preference,
        active: true,
        total-purchases: u0,
        average-rating: u5000
      }
    )
    (ok true)
  )
)

;; Energy trading functions
(define-public (create-energy-offer
  (energy-amount uint)
  (price-per-kwh uint)
  (delivery-time uint)
  (duration uint)
)
  (begin
    (asserts! (is-registered-producer tx-sender) err-unauthorized)
    (asserts! (> energy-amount u0) err-invalid-input)
    (asserts! (> delivery-time u1) err-invalid-input)
    
    (match (map-get? energy-producers { producer: tx-sender })
      producer
      (begin
        (asserts! (>= (get available-energy producer) energy-amount) err-insufficient-energy)
        
        (let 
          (
            (trade-id (+ (var-get trade-counter) u1))
            (total-price (* energy-amount price-per-kwh))
            (expiry-time (+ delivery-time duration))
          )
          (map-set energy-trades
            { trade-id: trade-id }
            {
              producer: tx-sender,
              consumer: tx-sender, ;; Will be updated when consumer accepts
              energy-amount: energy-amount,
              price-per-kwh: price-per-kwh,
              total-price: total-price,
              energy-source: (get energy-source producer),
              delivery-time: delivery-time,
              expiry-time: expiry-time,
              status: status-pending,
              grid-impact: (if (> energy-amount u1000) 100 10),
              created-at: u1,
              completed-at: none
            }
          )
          
          (var-set trade-counter trade-id)
          (ok trade-id)
        )
      )
      err-not-found
    )
  )
)

(define-public (accept-energy-trade (trade-id uint))
  (begin
    (asserts! (is-registered-consumer tx-sender) err-unauthorized)
    
    (match (map-get? energy-trades { trade-id: trade-id })
      trade
      (begin
        (asserts! (is-eq (get status trade) status-pending) err-invalid-input)
        (asserts! (> (get expiry-time trade) u1) err-trade-expired)
        
        ;; Check grid stability
        (asserts! (check-grid-stability (get grid-impact trade)) (ok true))
        
        ;; Update trade status
        (map-set energy-trades
          { trade-id: trade-id }
          (merge trade {
            consumer: tx-sender,
            status: status-active
          })
        )
        
        ;; Update producer's available energy
        (let ((producer-data (unwrap! (map-get? energy-producers { producer: (get producer trade) }) err-not-found)))
          (map-set energy-producers
            { producer: (get producer trade) }
            (merge producer-data {
              available-energy: (- (get available-energy producer-data) (get energy-amount trade)),
              total-trades: (+ (get total-trades producer-data) u1)
            })
          )
        )
        
        (ok true)
      )
      err-not-found
    )
  )
)

(define-public (complete-energy-trade (trade-id uint))
  (begin
    (asserts! (or (is-contract-owner) (is-grid-operator)) err-unauthorized)
    
    (match (map-get? energy-trades { trade-id: trade-id })
      trade
      (begin
        (asserts! (is-eq (get status trade) status-active) err-invalid-input)
        
        ;; Update trade status
        (map-set energy-trades
          { trade-id: trade-id }
          (merge trade {
            status: status-completed,
            completed-at: (some u1)
          })
        )
        
        ;; Update consumer statistics
        (let ((consumer-data (unwrap! (map-get? energy-consumers { consumer: (get consumer trade) }) err-not-found)))
          (map-set energy-consumers
            { consumer: (get consumer trade) }
            (merge consumer-data {
              total-purchases: (+ (get total-purchases consumer-data) u1)
            })
          )
        )
        
        (ok (get total-price trade))
      )
      err-not-found
    )
  )
)

;; Grid management functions
(define-public (update-grid-stability (new-load uint))
  (begin
    (asserts! (is-grid-operator) err-unauthorized)
    (asserts! (<= new-load u10000) err-invalid-input)
    
    (var-set current-grid-load new-load)
    
    ;; Record grid balancing data
    (map-set grid-balancing
      { timestamp: u1 }
      {
        total-supply: (calculate-total-supply),
        total-demand: (calculate-total-demand),
        grid-stability: new-load,
        renewable-percentage: (calculate-renewable-percentage),
        peak-demand: (> new-load u7000),
        balancing-actions: (list "LOAD_UPDATE")
      }
    )
    
    (ok true)
  )
)

;; Helper functions
(define-private (calculate-grid-impact (energy-amount uint))
  ;; Simplified grid impact calculation
  (if (> energy-amount u1000) 100 10)
)

(define-private (check-grid-stability (impact int))
  (> (var-get current-grid-load) (var-get grid-stability-threshold))
)

(define-private (calculate-total-supply)
  ;; Simplified total supply calculation
  u5000
)

(define-private (calculate-total-demand)
  ;; Simplified total demand calculation
  u4500
)

(define-private (calculate-renewable-percentage)
  ;; Simplified renewable percentage calculation
  u6500
)

;; Pricing functions
(define-public (update-energy-pricing
  (grid-location (string-ascii 10))
  (base-price uint)
  (peak-multiplier uint)
  (renewable-premium uint)
)
  (begin
    (asserts! (is-grid-operator) err-unauthorized)
    
    (map-set energy-pricing
      { grid-location: grid-location }
      {
        base-price: base-price,
        peak-multiplier: peak-multiplier,
        renewable-premium: renewable-premium,
        last-updated: u1
      }
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-energy-producer (producer principal))
  (map-get? energy-producers { producer: producer })
)

(define-read-only (get-energy-consumer (consumer principal))
  (map-get? energy-consumers { consumer: consumer })
)

(define-read-only (get-energy-trade (trade-id uint))
  (map-get? energy-trades { trade-id: trade-id })
)

(define-read-only (get-grid-balancing (timestamp uint))
  (map-get? grid-balancing { timestamp: timestamp })
)

(define-read-only (get-energy-pricing (grid-location (string-ascii 10)))
  (map-get? energy-pricing { grid-location: grid-location })
)

(define-read-only (get-current-grid-load)
  (var-get current-grid-load)
)

(define-read-only (get-trade-counter)
  (var-get trade-counter)
)

(define-read-only (calculate-trade-cost (energy-amount uint) (price-per-kwh uint))
  (* energy-amount price-per-kwh)
)
