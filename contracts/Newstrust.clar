

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ARTICLE_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_VOTED (err u102))
(define-constant ERR_INSUFFICIENT_STAKE (err u103))
(define-constant ERR_VOTING_CLOSED (err u104))
(define-constant ERR_ALREADY_FACT_CHECKED (err u105))
(define-constant ERR_INVALID_RATING (err u106))
(define-constant ERR_POOL_NOT_FOUND (err u107))
(define-constant ERR_INSUFFICIENT_POOL_BALANCE (err u108))
(define-constant ERR_POOL_ALREADY_EXISTS (err u109))
(define-constant ERR_INVALID_POOL_PARAMS (err u110))
(define-constant ERR_POOL_CLOSED (err u111))
(define-constant ERR_ALREADY_CONTRIBUTED (err u112))
(define-constant ERR_UPDATE_NOT_FOUND (err u113))
(define-constant ERR_INVALID_UPDATE_TYPE (err u114))
(define-constant ERR_UPDATE_ALREADY_VOTED (err u115))
(define-constant ERR_UPDATE_VOTING_CLOSED (err u116))
(define-constant ERR_UPDATE_NOT_PENDING (err u117))
(define-constant ERR_NOT_TRUSTED_ALERTER (err u118))
(define-constant ERR_ALERT_NOT_FOUND (err u119))
(define-constant ERR_ALREADY_SUBSCRIBED (err u120))
(define-constant ERR_NOT_SUBSCRIBED (err u121))
(define-constant ERR_INVALID_URGENCY_LEVEL (err u122))
(define-constant ERR_ALERT_EXPIRED (err u123))
(define-constant ERR_ALREADY_VERIFIED_SOURCE (err u124))
(define-constant ERR_INSUFFICIENT_VERIFICATIONS (err u125))

(define-constant MIN_STAKE u1000000)
(define-constant VOTING_PERIOD u144)
(define-constant FACT_CHECK_REWARD u500000)
(define-constant EDITORIAL_REWARD u300000)
(define-constant MIN_POOL_CONTRIBUTION u100000)
(define-constant POOL_DURATION u1008)
(define-constant MAX_POOL_CONTRIBUTORS u200)
(define-constant UPDATE_VOTING_PERIOD u72)
(define-constant UPDATE_STAKE u250000)
(define-constant ALERT_STAKE u500000)
(define-constant ALERT_EXPIRY_PERIOD u24)
(define-constant MIN_VERIFICATIONS u3)
(define-constant TRUSTED_ALERTER_MIN_REPUTATION u750)
(define-constant ALERT_REWARD u100000)

(define-data-var next-article-id uint u1)
(define-data-var platform-fee uint u50000)
(define-data-var next-pool-id uint u1)
(define-data-var next-update-id uint u1)
(define-data-var next-alert-id uint u1)

(define-map articles
  uint
  {
    author: principal,
    title: (string-ascii 200),
    content-hash: (string-ascii 64),
    timestamp: uint,
    stake: uint,
    status: (string-ascii 20),
    fact-check-score: uint,
    editorial-score: uint,
    voting-deadline: uint
  }
)

(define-map fact-checks
  {article-id: uint, checker: principal}
  {
    rating: uint,
    evidence-hash: (string-ascii 64),
    timestamp: uint,
    stake: uint
  }
)

(define-map editorial-votes
  {article-id: uint, voter: principal}
  {
    vote: bool,
    timestamp: uint,
    stake: uint
  }
)

(define-map user-reputation
  principal
  {
    fact-check-score: uint,
    editorial-score: uint,
    total-stake: uint,
    articles-submitted: uint
  }
)

(define-map article-fact-checkers
  uint
  (list 50 principal)
)

(define-map article-voters
  uint
  (list 100 principal)
)

(define-map reward-pools
  uint
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    total-amount: uint,
    current-amount: uint,
    quality-threshold: uint,
    created-at: uint,
    expires-at: uint,
    is-active: bool,
    distributed: bool
  }
)

(define-map pool-contributions
  {pool-id: uint, contributor: principal}
  {
    amount: uint,
    timestamp: uint
  }
)

(define-map pool-contributors
  uint
  (list 200 principal)
)

(define-map pool-article-rewards
  {pool-id: uint, article-id: uint}
  {
    author-reward: uint,
    fact-checker-bonus: uint,
    voter-bonus: uint,
    distributed: bool
  }
)

(define-map article-updates
  uint
  {
    article-id: uint,
    author: principal,
    update-type: (string-ascii 20),
    title: (string-ascii 200),
    content-hash: (string-ascii 64),
    reasoning: (string-ascii 500),
    timestamp: uint,
    voting-deadline: uint,
    status: (string-ascii 20),
    approval-score: uint,
    stake: uint
  }
)

(define-map update-votes
  {update-id: uint, voter: principal}
  {
    vote: bool,
    timestamp: uint,
    stake: uint
  }
)

(define-map update-voters
  uint
  (list 100 principal)
)

(define-map article-update-history
  uint
  (list 20 uint)
)

;; Breaking News Alert System Maps
(define-map breaking-news-alerts
  uint
  {
    alerter: principal,
    title: (string-ascii 200),
    summary: (string-ascii 500),
    content-hash: (string-ascii 64),
    urgency-level: uint, ;; 1=low, 2=medium, 3=high, 4=critical
    timestamp: uint,
    expires-at: uint,
    status: (string-ascii 20), ;; pending, verified, rejected, expired
    verification-count: uint,
    stake: uint
  }
)

(define-map alert-source-verifications
  {alert-id: uint, verifier: principal}
  {
    verified: bool,
    source-url: (string-ascii 200),
    evidence-hash: (string-ascii 64),
    timestamp: uint,
    stake: uint
  }
)

(define-map alert-subscriptions
  {subscriber: principal, urgency-level: uint}
  {
    subscribed-at: uint,
    is-active: bool,
    notifications-received: uint
  }
)

(define-map trusted-alerters
  principal
  {
    promoted-at: uint,
    alerts-submitted: uint,
    accuracy-rate: uint,
    is-active: bool
  }
)

(define-map alert-verifiers
  uint
  (list 20 principal)
)

(define-map alert-subscribers
  {alert-id: uint, urgency-level: uint}
  (list 500 principal)
)

(define-public (submit-article (title (string-ascii 200)) (content-hash (string-ascii 64)))
  (let
    (
      (article-id (var-get next-article-id))
      (current-height stacks-block-height)
    )
    (asserts! (>= (stx-get-balance tx-sender) MIN_STAKE) ERR_INSUFFICIENT_STAKE)
    (try! (stx-transfer? MIN_STAKE tx-sender (as-contract tx-sender)))
    (map-set articles article-id
      {
        author: tx-sender,
        title: title,
        content-hash: content-hash,
        timestamp: current-height,
        stake: MIN_STAKE,
        status: "pending",
        fact-check-score: u0,
        editorial-score: u0,
        voting-deadline: (+ current-height VOTING_PERIOD)
      }
    )
    (map-set user-reputation tx-sender
      (merge
        (default-to
          {fact-check-score: u0, editorial-score: u0, total-stake: u0, articles-submitted: u0}
          (map-get? user-reputation tx-sender)
        )
        {articles-submitted: (+ (get articles-submitted (default-to {fact-check-score: u0, editorial-score: u0, total-stake: u0, articles-submitted: u0} (map-get? user-reputation tx-sender))) u1)}
      )
    )
    (var-set next-article-id (+ article-id u1))
    (ok article-id)
  )
)

(define-public (fact-check-article (article-id uint) (rating uint) (evidence-hash (string-ascii 64)))
  (let
    (
      (article (unwrap! (map-get? articles article-id) ERR_ARTICLE_NOT_FOUND))
      (current-height stacks-block-height)
      (stake-amount (/ MIN_STAKE u2))
    )
    (asserts! (<= rating u100) ERR_INVALID_RATING)
    (asserts! (< current-height (get voting-deadline article)) ERR_VOTING_CLOSED)
    (asserts! (is-none (map-get? fact-checks {article-id: article-id, checker: tx-sender})) ERR_ALREADY_FACT_CHECKED)
    (asserts! (>= (stx-get-balance tx-sender) stake-amount) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set fact-checks {article-id: article-id, checker: tx-sender}
      {
        rating: rating,
        evidence-hash: evidence-hash,
        timestamp: current-height,
        stake: stake-amount
      }
    )
    
    (map-set article-fact-checkers article-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? article-fact-checkers article-id)) tx-sender) u50) ERR_NOT_AUTHORIZED)
    )
    
    (update-fact-check-score article-id)
    (ok true)
  )
)

(define-public (editorial-vote (article-id uint) (vote bool))
  (let
    (
      (article (unwrap! (map-get? articles article-id) ERR_ARTICLE_NOT_FOUND))
      (current-height stacks-block-height)
      (stake-amount (/ MIN_STAKE u4))
    )
    (asserts! (< current-height (get voting-deadline article)) ERR_VOTING_CLOSED)
    (asserts! (is-none (map-get? editorial-votes {article-id: article-id, voter: tx-sender})) ERR_ALREADY_VOTED)
    (asserts! (>= (stx-get-balance tx-sender) stake-amount) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set editorial-votes {article-id: article-id, voter: tx-sender}
      {
        vote: vote,
        timestamp: current-height,
        stake: stake-amount
      }
    )
    
    (map-set article-voters article-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? article-voters article-id)) tx-sender) u100) ERR_NOT_AUTHORIZED)
    )
    
    (update-editorial-score article-id)
    (ok true)
  )
)

(define-public (finalize-article (article-id uint))
  (let
    (
      (article (unwrap! (map-get? articles article-id) ERR_ARTICLE_NOT_FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (>= current-height (get voting-deadline article)) ERR_VOTING_CLOSED)
    
    (let
      (
        (fact-score (get fact-check-score article))
        (editorial-score (get editorial-score article))
        (final-status (if (and (>= fact-score u60) (>= editorial-score u60)) "approved" "rejected"))
      )
      (let
        (
          (set-result (map-set articles article-id (merge article {status: final-status})))
          (reward-result (distribute-rewards article-id final-status))
        )
        (ok final-status)
      )
    )
  )
)

(define-private (update-fact-check-score (article-id uint))
  (let
    (
      (checkers (default-to (list) (map-get? article-fact-checkers article-id)))
      (total-rating (fold calculate-fact-check-total checkers u0))
      (checker-count (len checkers))
      (average-score (if (> checker-count u0) (/ total-rating checker-count) u0))
    )
    (map-set articles article-id
      (merge
        (unwrap-panic (map-get? articles article-id))
        {fact-check-score: average-score}
      )
    )
  )
)

(define-private (calculate-fact-check-total (checker principal) (acc uint))
  (let
    (
      (article-id (var-get next-article-id))
      (fact-check (map-get? fact-checks {article-id: (- article-id u1), checker: checker}))
    )
    (match fact-check
      check (+ acc (get rating check))
      acc
    )
  )
)

(define-private (update-editorial-score (article-id uint))
  (let
    (
      (voters (default-to (list) (map-get? article-voters article-id)))
      (positive-votes (fold calculate-positive-votes voters u0))
      (total-votes (len voters))
      (approval-rate (if (> total-votes u0) (/ (* positive-votes u100) total-votes) u0))
    )
    (map-set articles article-id
      (merge
        (unwrap-panic (map-get? articles article-id))
        {editorial-score: approval-rate}
      )
    )
  )
)

(define-private (calculate-positive-votes (voter principal) (acc uint))
  (let
    (
      (article-id (- (var-get next-article-id) u1))
      (vote-data (map-get? editorial-votes {article-id: article-id, voter: voter}))
    )
    (match vote-data
      vote (if (get vote vote) (+ acc u1) acc)
      acc
    )
  )
)

(define-private (distribute-rewards (article-id uint) (status (string-ascii 20)))
  (let
    (
      (article (unwrap-panic (map-get? articles article-id)))
      (checkers (default-to (list) (map-get? article-fact-checkers article-id)))
      (voters (default-to (list) (map-get? article-voters article-id)))
    )
    (if (is-eq status "approved")
      (begin
        (try! (as-contract (stx-transfer? (get stake article) tx-sender (get author article))))
        (fold reward-fact-checker checkers true)
        (fold reward-voter voters true)
        (ok true)
      )
      (begin
        (fold reward-fact-checker checkers true)
        (fold reward-voter voters true)
        (ok true)
      )
    )
  )
)

(define-private (reward-fact-checker (checker principal) (acc bool))
  (begin
    (unwrap! (as-contract (stx-transfer? FACT_CHECK_REWARD tx-sender checker)) false)
    acc
  )
)

(define-private (reward-voter (voter principal) (acc bool))
  (begin
    (unwrap! (as-contract (stx-transfer? EDITORIAL_REWARD tx-sender voter)) false)
    acc
  )
)

(define-read-only (get-article (article-id uint))
  (map-get? articles article-id)
)

(define-read-only (get-fact-check (article-id uint) (checker principal))
  (map-get? fact-checks {article-id: article-id, checker: checker})
)

(define-read-only (get-editorial-vote (article-id uint) (voter principal))
  (map-get? editorial-votes {article-id: article-id, voter: voter})
)

(define-read-only (get-user-reputation (user principal))
  (default-to
    {fact-check-score: u0, editorial-score: u0, total-stake: u0, articles-submitted: u0}
    (map-get? user-reputation user)
  )
)

(define-read-only (get-article-checkers (article-id uint))
  (default-to (list) (map-get? article-fact-checkers article-id))
)

(define-read-only (get-article-voters (article-id uint))
  (default-to (list) (map-get? article-voters article-id))
)

(define-read-only (get-next-article-id)
  (var-get next-article-id)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set platform-fee new-fee)
    (ok true)
  )
)

(define-public (create-reward-pool (title (string-ascii 100)) (description (string-ascii 500)) (quality-threshold uint) (initial-amount uint))
  (let
    (
      (pool-id (var-get next-pool-id))
      (current-height stacks-block-height)
    )
    (asserts! (>= quality-threshold u50) ERR_INVALID_POOL_PARAMS)
    (asserts! (<= quality-threshold u100) ERR_INVALID_POOL_PARAMS)
    (asserts! (>= initial-amount MIN_POOL_CONTRIBUTION) ERR_INVALID_POOL_PARAMS)
    (asserts! (>= (stx-get-balance tx-sender) initial-amount) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? initial-amount tx-sender (as-contract tx-sender)))
    
    (map-set reward-pools pool-id
      {
        creator: tx-sender,
        title: title,
        description: description,
        total-amount: initial-amount,
        current-amount: initial-amount,
        quality-threshold: quality-threshold,
        created-at: current-height,
        expires-at: (+ current-height POOL_DURATION),
        is-active: true,
        distributed: false
      }
    )
    
    (map-set pool-contributions {pool-id: pool-id, contributor: tx-sender}
      {
        amount: initial-amount,
        timestamp: current-height
      }
    )
    
    (map-set pool-contributors pool-id
      (unwrap! (as-max-len? (list tx-sender) u200) ERR_NOT_AUTHORIZED)
    )
    
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

(define-public (contribute-to-pool (pool-id uint) (amount uint))
  (let
    (
      (pool (unwrap! (map-get? reward-pools pool-id) ERR_POOL_NOT_FOUND))
      (current-height stacks-block-height)
      (existing-contribution (map-get? pool-contributions {pool-id: pool-id, contributor: tx-sender}))
    )
    (asserts! (get is-active pool) ERR_POOL_CLOSED)
    (asserts! (< current-height (get expires-at pool)) ERR_POOL_CLOSED)
    (asserts! (>= amount MIN_POOL_CONTRIBUTION) ERR_INVALID_POOL_PARAMS)
    (asserts! (>= (stx-get-balance tx-sender) amount) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none existing-contribution) ERR_ALREADY_CONTRIBUTED)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set reward-pools pool-id
      (merge pool
        {
          total-amount: (+ (get total-amount pool) amount),
          current-amount: (+ (get current-amount pool) amount)
        }
      )
    )
    
    (map-set pool-contributions {pool-id: pool-id, contributor: tx-sender}
      {
        amount: amount,
        timestamp: current-height
      }
    )
    
    (map-set pool-contributors pool-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? pool-contributors pool-id)) tx-sender) u200) ERR_NOT_AUTHORIZED)
    )
    
    (ok true)
  )
)

(define-public (claim-pool-reward (pool-id uint) (article-id uint))
  (let
    (
      (pool (unwrap! (map-get? reward-pools pool-id) ERR_POOL_NOT_FOUND))
      (article (unwrap! (map-get? articles article-id) ERR_ARTICLE_NOT_FOUND))
      (current-height stacks-block-height)
      (existing-reward (map-get? pool-article-rewards {pool-id: pool-id, article-id: article-id}))
    )
    (asserts! (get is-active pool) ERR_POOL_CLOSED)
    (asserts! (is-eq (get status article) "approved") ERR_NOT_AUTHORIZED)
    (asserts! (>= (get fact-check-score article) (get quality-threshold pool)) ERR_NOT_AUTHORIZED)
    (asserts! (>= (get editorial-score article) (get quality-threshold pool)) ERR_NOT_AUTHORIZED)
    (asserts! (is-none existing-reward) ERR_NOT_AUTHORIZED)
    (asserts! (> (get current-amount pool) u0) ERR_INSUFFICIENT_POOL_BALANCE)
    
    (let
      (
        (pool-third (/ (get current-amount pool) u3))
        (total-reward (if (< pool-third u2000000) pool-third u2000000))
        (author-reward (/ (* total-reward u60) u100))
        (fact-checker-bonus (/ (* total-reward u25) u100))
        (voter-bonus (/ (* total-reward u15) u100))
      )
      (map-set pool-article-rewards {pool-id: pool-id, article-id: article-id}
        {
          author-reward: author-reward,
          fact-checker-bonus: fact-checker-bonus,
          voter-bonus: voter-bonus,
          distributed: false
        }
      )
      
      (map-set reward-pools pool-id
        (merge pool {current-amount: (- (get current-amount pool) total-reward)})
      )
      
      (ok {author-reward: author-reward, fact-checker-bonus: fact-checker-bonus, voter-bonus: voter-bonus})
    )
  )
)

(define-public (distribute-pool-rewards (pool-id uint) (article-id uint))
  (let
    (
      (pool (unwrap! (map-get? reward-pools pool-id) ERR_POOL_NOT_FOUND))
      (article (unwrap! (map-get? articles article-id) ERR_ARTICLE_NOT_FOUND))
      (reward-data (unwrap! (map-get? pool-article-rewards {pool-id: pool-id, article-id: article-id}) ERR_NOT_AUTHORIZED))
      (checkers (default-to (list) (map-get? article-fact-checkers article-id)))
      (voters (default-to (list) (map-get? article-voters article-id)))
    )
    (asserts! (not (get distributed reward-data)) ERR_NOT_AUTHORIZED)
    
    (try! (as-contract (stx-transfer? (get author-reward reward-data) tx-sender (get author article))))
    
    (let
      (
        (checker-count (len checkers))
        (voter-count (len voters))
        (individual-checker-bonus (if (> checker-count u0) (/ (get fact-checker-bonus reward-data) checker-count) u0))
        (individual-voter-bonus (if (> voter-count u0) (/ (get voter-bonus reward-data) voter-count) u0))
      )
      (distribute-checker-bonuses checkers individual-checker-bonus)
      (distribute-voter-bonuses voters individual-voter-bonus)
      
      (map-set pool-article-rewards {pool-id: pool-id, article-id: article-id}
        (merge reward-data {distributed: true})
      )
      
      (ok true)
    )
  )
)

(define-private (distribute-checker-bonuses (checkers (list 50 principal)) (bonus uint))
  (if (> bonus u0)
    (begin
      (fold distribute-single-checker-bonus checkers bonus)
      true
    )
    true
  )
)

(define-private (distribute-voter-bonuses (voters (list 100 principal)) (bonus uint))
  (if (> bonus u0)
    (begin
      (fold distribute-single-voter-bonus voters bonus)
      true
    )
    true
  )
)

(define-private (distribute-single-checker-bonus (checker principal) (bonus uint))
  (begin
    (unwrap! (as-contract (stx-transfer? bonus tx-sender checker)) bonus)
    bonus
  )
)

(define-private (distribute-single-voter-bonus (voter principal) (bonus uint))
  (begin
    (unwrap! (as-contract (stx-transfer? bonus tx-sender voter)) bonus)
    bonus
  )
)

(define-public (close-expired-pool (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? reward-pools pool-id) ERR_POOL_NOT_FOUND))
      (current-height stacks-block-height)
      (contributors (default-to (list) (map-get? pool-contributors pool-id)))
    )
    (asserts! (>= current-height (get expires-at pool)) ERR_POOL_CLOSED)
    (asserts! (get is-active pool) ERR_NOT_AUTHORIZED)
    
    (let
      (
        (remaining-amount (get current-amount pool))
        (contributor-count (len contributors))
        (refund-per-contributor (if (> contributor-count u0) (/ remaining-amount contributor-count) u0))
      )
      (fold refund-contributor contributors true)
      
      (map-set reward-pools pool-id
        (merge pool {is-active: false, current-amount: u0})
      )
      
      (ok true)
    )
  )
)

(define-private (refund-contributor (contributor principal) (acc bool))
  (begin
    (unwrap! (as-contract (stx-transfer? u0 tx-sender contributor)) false)
    acc
  )
)

(define-read-only (get-reward-pool (pool-id uint))
  (map-get? reward-pools pool-id)
)

(define-read-only (get-pool-contribution (pool-id uint) (contributor principal))
  (map-get? pool-contributions {pool-id: pool-id, contributor: contributor})
)

(define-read-only (get-pool-contributors (pool-id uint))
  (default-to (list) (map-get? pool-contributors pool-id))
)

(define-read-only (get-pool-article-reward (pool-id uint) (article-id uint))
  (map-get? pool-article-rewards {pool-id: pool-id, article-id: article-id})
)

(define-read-only (get-next-pool-id)
  (var-get next-pool-id)
)

(define-read-only (get-active-pools)
  (ok true)
)

(define-public (submit-article-update (article-id uint) (update-type (string-ascii 20)) (title (string-ascii 200)) (content-hash (string-ascii 64)) (reasoning (string-ascii 500)))
  (let
    (
      (article (unwrap! (map-get? articles article-id) ERR_ARTICLE_NOT_FOUND))
      (update-id (var-get next-update-id))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq (get author article) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status article) "approved") ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq update-type "correction") (is-eq update-type "clarification") (is-eq update-type "retraction")) ERR_INVALID_UPDATE_TYPE)
    (asserts! (>= (stx-get-balance tx-sender) UPDATE_STAKE) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? UPDATE_STAKE tx-sender (as-contract tx-sender)))
    
    (map-set article-updates update-id
      {
        article-id: article-id,
        author: tx-sender,
        update-type: update-type,
        title: title,
        content-hash: content-hash,
        reasoning: reasoning,
        timestamp: current-height,
        voting-deadline: (+ current-height UPDATE_VOTING_PERIOD),
        status: "pending",
        approval-score: u0,
        stake: UPDATE_STAKE
      }
    )
    
    (var-set next-update-id (+ update-id u1))
    (ok update-id)
  )
)

(define-public (vote-on-update (update-id uint) (vote bool))
  (let
    (
      (update-data (unwrap! (map-get? article-updates update-id) ERR_UPDATE_NOT_FOUND))
      (current-height stacks-block-height)
      (stake-amount (/ UPDATE_STAKE u4))
    )
    (asserts! (< current-height (get voting-deadline update-data)) ERR_UPDATE_VOTING_CLOSED)
    (asserts! (is-eq (get status update-data) "pending") ERR_UPDATE_NOT_PENDING)
    (asserts! (is-none (map-get? update-votes {update-id: update-id, voter: tx-sender})) ERR_UPDATE_ALREADY_VOTED)
    (asserts! (>= (stx-get-balance tx-sender) stake-amount) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set update-votes {update-id: update-id, voter: tx-sender}
      {
        vote: vote,
        timestamp: current-height,
        stake: stake-amount
      }
    )
    
    (map-set update-voters update-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? update-voters update-id)) tx-sender) u100) ERR_NOT_AUTHORIZED)
    )
    
    (update-approval-score update-id)
    (ok true)
  )
)

(define-public (finalize-update (update-id uint))
  (let
    (
      (update-data (unwrap! (map-get? article-updates update-id) ERR_UPDATE_NOT_FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (>= current-height (get voting-deadline update-data)) ERR_UPDATE_VOTING_CLOSED)
    (asserts! (is-eq (get status update-data) "pending") ERR_UPDATE_NOT_PENDING)
    
    (let
      (
        (approval-score (get approval-score update-data))
        (final-status (if (>= approval-score u60) "approved" "rejected"))
        (article-id (get article-id update-data))
      )
      (map-set article-updates update-id
        (merge update-data {status: final-status})
      )
      
      (if (is-eq final-status "approved")
        (begin
          (try! (add-to-update-history article-id update-id))
          (try! (distribute-update-rewards update-id))
          (ok "approved")
        )
        (begin
          (try! (distribute-update-rewards update-id))
          (ok "rejected")
        )
      )
    )
  )
)

(define-private (update-approval-score (update-id uint))
  (let
    (
      (voters (default-to (list) (map-get? update-voters update-id)))
      (positive-votes (fold calculate-update-positive-votes voters u0))
      (total-votes (len voters))
      (approval-rate (if (> total-votes u0) (/ (* positive-votes u100) total-votes) u0))
    )
    (map-set article-updates update-id
      (merge
        (unwrap-panic (map-get? article-updates update-id))
        {approval-score: approval-rate}
      )
    )
  )
)

(define-private (calculate-update-positive-votes (voter principal) (acc uint))
  (let
    (
      (update-id (- (var-get next-update-id) u1))
      (vote-data (map-get? update-votes {update-id: update-id, voter: voter}))
    )
    (match vote-data
      vote (if (get vote vote) (+ acc u1) acc)
      acc
    )
  )
)

(define-private (add-to-update-history (article-id uint) (update-id uint))
  (let
    (
      (current-history (default-to (list) (map-get? article-update-history article-id)))
    )
    (map-set article-update-history article-id
      (unwrap! (as-max-len? (append current-history update-id) u20) (err u999))
    )
    (ok true)
  )
)

(define-private (distribute-update-rewards (update-id uint))
  (let
    (
      (update-data (unwrap-panic (map-get? article-updates update-id)))
      (voters (default-to (list) (map-get? update-voters update-id)))
      (status (get status update-data))
    )
    (if (is-eq status "approved")
      (begin
        (try! (as-contract (stx-transfer? (get stake update-data) tx-sender (get author update-data))))
        (fold reward-update-voter voters true)
        (ok true)
      )
      (begin
        (fold reward-update-voter voters true)
        (ok true)
      )
    )
  )
)

(define-private (reward-update-voter (voter principal) (acc bool))
  (begin
    (unwrap! (as-contract (stx-transfer? u50000 tx-sender voter)) false)
    acc
  )
)

(define-read-only (get-article-updates (article-id uint))
  (default-to (list) (map-get? article-update-history article-id))
)

(define-read-only (get-article-update (update-id uint))
  (map-get? article-updates update-id)
)

(define-read-only (get-update-vote (update-id uint) (voter principal))
  (map-get? update-votes {update-id: update-id, voter: voter})
)

(define-read-only (get-update-voters (update-id uint))
  (default-to (list) (map-get? update-voters update-id))
)

(define-read-only (get-next-update-id)
  (var-get next-update-id)
)

;; Breaking News Alert System Functions

;; Submit a breaking news alert (trusted alerters or high reputation users)
(define-public (submit-breaking-alert (title (string-ascii 200)) (summary (string-ascii 500)) (content-hash (string-ascii 64)) (urgency-level uint))
  (let
    (
      (alert-id (var-get next-alert-id))
      (current-height stacks-block-height)
      (user-rep (get-user-reputation tx-sender))
      (is-trusted (is-some (map-get? trusted-alerters tx-sender)))
    )
    (asserts! (and (> urgency-level u0) (<= urgency-level u4)) ERR_INVALID_URGENCY_LEVEL)
    (asserts! (or is-trusted (>= (+ (get fact-check-score user-rep) (get editorial-score user-rep)) TRUSTED_ALERTER_MIN_REPUTATION)) ERR_NOT_TRUSTED_ALERTER)
    (asserts! (>= (stx-get-balance tx-sender) ALERT_STAKE) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? ALERT_STAKE tx-sender (as-contract tx-sender)))
    
    (map-set breaking-news-alerts alert-id
      {
        alerter: tx-sender,
        title: title,
        summary: summary,
        content-hash: content-hash,
        urgency-level: urgency-level,
        timestamp: current-height,
        expires-at: (+ current-height ALERT_EXPIRY_PERIOD),
        status: "pending",
        verification-count: u0,
        stake: ALERT_STAKE
      }
    )
    
    (var-set next-alert-id (+ alert-id u1))
    (ok alert-id)
  )
)

;; Verify alert source (community members)
(define-public (verify-alert-source (alert-id uint) (verified bool) (source-url (string-ascii 200)) (evidence-hash (string-ascii 64)))
  (let
    (
      (alert (unwrap! (map-get? breaking-news-alerts alert-id) ERR_ALERT_NOT_FOUND))
      (current-height stacks-block-height)
      (stake-amount (/ ALERT_STAKE u4))
    )
    (asserts! (< current-height (get expires-at alert)) ERR_ALERT_EXPIRED)
    (asserts! (is-eq (get status alert) "pending") ERR_ALERT_EXPIRED)
    (asserts! (is-none (map-get? alert-source-verifications {alert-id: alert-id, verifier: tx-sender})) ERR_ALREADY_VERIFIED_SOURCE)
    (asserts! (>= (stx-get-balance tx-sender) stake-amount) ERR_INSUFFICIENT_STAKE)
    
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set alert-source-verifications {alert-id: alert-id, verifier: tx-sender}
      {
        verified: verified,
        source-url: source-url,
        evidence-hash: evidence-hash,
        timestamp: current-height,
        stake: stake-amount
      }
    )
    
    (map-set alert-verifiers alert-id
      (unwrap! (as-max-len? (append (default-to (list) (map-get? alert-verifiers alert-id)) tx-sender) u20) ERR_NOT_AUTHORIZED)
    )
    
    (update-alert-verification-status alert-id)
    (ok true)
  )
)

;; Subscribe to breaking news alerts by urgency level
(define-public (subscribe-to-alerts (urgency-level uint))
  (let
    (
      (current-height stacks-block-height)
      (existing-subscription (map-get? alert-subscriptions {subscriber: tx-sender, urgency-level: urgency-level}))
    )
    (asserts! (and (> urgency-level u0) (<= urgency-level u4)) ERR_INVALID_URGENCY_LEVEL)
    (asserts! (is-none existing-subscription) ERR_ALREADY_SUBSCRIBED)
    
    (map-set alert-subscriptions {subscriber: tx-sender, urgency-level: urgency-level}
      {
        subscribed-at: current-height,
        is-active: true,
        notifications-received: u0
      }
    )
    (ok true)
  )
)

;; Promote user to trusted alerter (contract owner only)
(define-public (promote-to-trusted-alerter (user principal))
  (let
    (
      (current-height stacks-block-height)
      (user-rep (get-user-reputation user))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (>= (+ (get fact-check-score user-rep) (get editorial-score user-rep)) TRUSTED_ALERTER_MIN_REPUTATION) ERR_NOT_AUTHORIZED)
    (asserts! (is-none (map-get? trusted-alerters user)) ERR_ALREADY_SUBSCRIBED)
    
    (map-set trusted-alerters user
      {
        promoted-at: current-height,
        alerts-submitted: u0,
        accuracy-rate: u100,
        is-active: true
      }
    )
    (ok true)
  )
)

;; Update alert verification status based on verifier consensus
(define-private (update-alert-verification-status (alert-id uint))
  (let
    (
      (verifiers (default-to (list) (map-get? alert-verifiers alert-id)))
      (positive-verifications (fold calculate-positive-verifications verifiers u0))
      (total-verifications (len verifiers))
    )
    (if (>= positive-verifications MIN_VERIFICATIONS)
      (begin
        (map-set breaking-news-alerts alert-id
          (merge
            (unwrap-panic (map-get? breaking-news-alerts alert-id))
            {status: "verified", verification-count: positive-verifications}
          )
        )
        (unwrap-panic (distribute-to-subscribers alert-id))
        true
      )
      (if (>= total-verifications MIN_VERIFICATIONS)
        (begin
          (map-set breaking-news-alerts alert-id
            (merge
              (unwrap-panic (map-get? breaking-news-alerts alert-id))
              {status: "rejected", verification-count: positive-verifications}
            )
          )
          true
        )
        true
      )
    )
  )
)

(define-private (calculate-positive-verifications (verifier principal) (acc uint))
  (let
    (
      (alert-id (- (var-get next-alert-id) u1))
      (verification (map-get? alert-source-verifications {alert-id: alert-id, verifier: verifier}))
    )
    (match verification
      verify (if (get verified verify) (+ acc u1) acc)
      acc
    )
  )
)

;; Distribute alert to subscribers
(define-private (distribute-to-subscribers (alert-id uint))
  (let
    (
      (alert (unwrap-panic (map-get? breaking-news-alerts alert-id)))
      (urgency-level (get urgency-level alert))
    )
    ;; Reward alerter for successful alert
    (try! (as-contract (stx-transfer? ALERT_REWARD tx-sender (get alerter alert))))
    ;; In a real implementation, this would trigger notifications to all subscribers of this urgency level
    (ok true)
  )
)

;; Read-only functions for breaking news alerts
(define-read-only (get-breaking-alert (alert-id uint))
  (map-get? breaking-news-alerts alert-id)
)

(define-read-only (get-alert-verification (alert-id uint) (verifier principal))
  (map-get? alert-source-verifications {alert-id: alert-id, verifier: verifier})
)

(define-read-only (get-alert-subscription (subscriber principal) (urgency-level uint))
  (map-get? alert-subscriptions {subscriber: subscriber, urgency-level: urgency-level})
)

(define-read-only (get-trusted-alerter (user principal))
  (map-get? trusted-alerters user)
)

(define-read-only (is-trusted-alerter (user principal))
  (is-some (map-get? trusted-alerters user))
)

(define-read-only (get-alert-verifiers (alert-id uint))
  (default-to (list) (map-get? alert-verifiers alert-id))
)

(define-read-only (get-next-alert-id)
  (var-get next-alert-id)
)

