;; Transparent Talent Contest
;; A decentralized platform for talent contests with transparent voting and automatic prize distribution

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_CONTEST_NOT_FOUND (err u101))
(define-constant ERR_CONTEST_NOT_ACTIVE (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_ENTRY_NOT_FOUND (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_CONTEST_ENDED (err u106))
(define-constant ERR_CONTEST_NOT_ENDED (err u107))
(define-constant ERR_ALREADY_CLAIMED (err u108))
(define-constant ERR_NOT_WINNER (err u109))
(define-constant ERR_INVALID_PERCENTAGE (err u110))
(define-constant ERR_NO_ENTRIES (err u111))
(define-constant ERR_ALREADY_FINALIZED (err u112))

(define-data-var contest-id-nonce uint u0)
(define-data-var entry-id-nonce uint u0)

(define-map contests
  { contest-id: uint }
  {
    title: (string-ascii 50),
    category: (string-ascii 20),
    creator: principal,
    start-block: uint,
    end-block: uint,
    entry-fee: uint,
    prize-pool: uint,
    winner-percentage: uint,
    runner-up-percentage: uint,
    is-finalized: bool,
    total-entries: uint,
    winner-entry-id: (optional uint),
    runner-up-entry-id: (optional uint)
  }
)

(define-map entries
  { entry-id: uint }
  {
    contest-id: uint,
    participant: principal,
    title: (string-ascii 50),
    description: (string-ascii 200),
    media-url: (string-ascii 100),
    votes: uint,
    submission-block: uint
  }
)

(define-map votes
  { contest-id: uint, voter: principal }
  { entry-id: uint, vote-block: uint }
)

(define-map prize-claims
  { contest-id: uint, participant: principal }
  { claimed: bool, amount: uint }
)

(define-map user-entries
  { contest-id: uint, participant: principal }
  { entry-id: uint }
)

(define-public (create-contest 
  (title (string-ascii 50))
  (category (string-ascii 20))
  (duration-blocks uint)
  (entry-fee uint)
  (winner-percentage uint)
  (runner-up-percentage uint))
  (let ((contest-id (+ (var-get contest-id-nonce) u1))
        (start-block stacks-block-height)
        (end-block (+ stacks-block-height duration-blocks)))
    (asserts! (and (> winner-percentage u0) (<= winner-percentage u100)) ERR_INVALID_PERCENTAGE)
    (asserts! (and (>= runner-up-percentage u0) (<= runner-up-percentage u100)) ERR_INVALID_PERCENTAGE)
    (asserts! (<= (+ winner-percentage runner-up-percentage) u100) ERR_INVALID_PERCENTAGE)
    (map-set contests
      { contest-id: contest-id }
      {
        title: title,
        category: category,
        creator: tx-sender,
        start-block: start-block,
        end-block: end-block,
        entry-fee: entry-fee,
        prize-pool: u0,
        winner-percentage: winner-percentage,
        runner-up-percentage: runner-up-percentage,
        is-finalized: false,
        total-entries: u0,
        winner-entry-id: none,
        runner-up-entry-id: none
      })
    (var-set contest-id-nonce contest-id)
    (ok contest-id)))

(define-public (submit-entry
  (contest-id uint)
  (title (string-ascii 50))
  (description (string-ascii 200))
  (media-url (string-ascii 100)))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR_CONTEST_NOT_FOUND))
        (entry-id (+ (var-get entry-id-nonce) u1))
        (entry-fee (get entry-fee contest)))
    (asserts! (>= stacks-block-height (get start-block contest)) ERR_CONTEST_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get end-block contest)) ERR_CONTEST_ENDED)
    (if (> entry-fee u0)
      (try! (stx-transfer? entry-fee tx-sender (as-contract tx-sender)))
      true)
    (map-set entries
      { entry-id: entry-id }
      {
        contest-id: contest-id,
        participant: tx-sender,
        title: title,
        description: description,
        media-url: media-url,
        votes: u0,
        submission-block: stacks-block-height
      })
    (map-set user-entries
      { contest-id: contest-id, participant: tx-sender }
      { entry-id: entry-id })
    (map-set contests
      { contest-id: contest-id }
      (merge contest {
        prize-pool: (+ (get prize-pool contest) entry-fee),
        total-entries: (+ (get total-entries contest) u1)
      }))
    (var-set entry-id-nonce entry-id)
    (ok entry-id)))

(define-public (vote-for-entry (contest-id uint) (entry-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR_CONTEST_NOT_FOUND))
        (entry (unwrap! (map-get? entries { entry-id: entry-id }) ERR_ENTRY_NOT_FOUND))
        (existing-vote (map-get? votes { contest-id: contest-id, voter: tx-sender })))
    (asserts! (>= stacks-block-height (get start-block contest)) ERR_CONTEST_NOT_ACTIVE)
    (asserts! (< stacks-block-height (get end-block contest)) ERR_CONTEST_ENDED)
    (asserts! (is-eq (get contest-id entry) contest-id) ERR_ENTRY_NOT_FOUND)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (map-set votes
      { contest-id: contest-id, voter: tx-sender }
      { entry-id: entry-id, vote-block: stacks-block-height })
    (map-set entries
      { entry-id: entry-id }
      (merge entry { votes: (+ (get votes entry) u1) }))
    (ok true)))

(define-public (finalize-contest (contest-id uint) (winner-entry-id uint) (runner-up-entry-id (optional uint)))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR_CONTEST_NOT_FOUND))
        (winner-entry (unwrap! (map-get? entries { entry-id: winner-entry-id }) ERR_ENTRY_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator contest)) ERR_UNAUTHORIZED)
    (asserts! (>= stacks-block-height (get end-block contest)) ERR_CONTEST_NOT_ENDED)
    (asserts! (not (get is-finalized contest)) ERR_ALREADY_FINALIZED)
    (asserts! (> (get total-entries contest) u0) ERR_NO_ENTRIES)
    (asserts! (is-eq (get contest-id winner-entry) contest-id) ERR_ENTRY_NOT_FOUND)
    (if (is-some runner-up-entry-id)
      (let ((runner-up-entry (unwrap! (map-get? entries { entry-id: (unwrap-panic runner-up-entry-id) }) ERR_ENTRY_NOT_FOUND)))
        (asserts! (is-eq (get contest-id runner-up-entry) contest-id) ERR_ENTRY_NOT_FOUND)
        (asserts! (not (is-eq winner-entry-id (unwrap-panic runner-up-entry-id))) ERR_ENTRY_NOT_FOUND))
      true)
    (map-set contests
      { contest-id: contest-id }
      (merge contest { 
        is-finalized: true,
        winner-entry-id: (some winner-entry-id),
        runner-up-entry-id: runner-up-entry-id
      }))
    (ok true)))

(define-public (claim-winner-prize (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR_CONTEST_NOT_FOUND))
        (user-entry (unwrap! (map-get? user-entries { contest-id: contest-id, participant: tx-sender }) ERR_NOT_WINNER))
        (prize-pool (get prize-pool contest))
        (winner-amount (/ (* prize-pool (get winner-percentage contest)) u100))
        (existing-claim (map-get? prize-claims { contest-id: contest-id, participant: tx-sender })))
    (asserts! (get is-finalized contest) ERR_CONTEST_NOT_ENDED)
    (asserts! (is-none existing-claim) ERR_ALREADY_CLAIMED)
    (asserts! (is-eq (get winner-entry-id contest) (some (get entry-id user-entry))) ERR_NOT_WINNER)
    (map-set prize-claims
      { contest-id: contest-id, participant: tx-sender }
      { claimed: true, amount: winner-amount })
    (as-contract (stx-transfer? winner-amount tx-sender tx-sender))))

(define-public (claim-runner-up-prize (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR_CONTEST_NOT_FOUND))
        (user-entry (unwrap! (map-get? user-entries { contest-id: contest-id, participant: tx-sender }) ERR_NOT_WINNER))
        (prize-pool (get prize-pool contest))
        (runner-up-amount (/ (* prize-pool (get runner-up-percentage contest)) u100))
        (existing-claim (map-get? prize-claims { contest-id: contest-id, participant: tx-sender })))
    (asserts! (get is-finalized contest) ERR_CONTEST_NOT_ENDED)
    (asserts! (is-none existing-claim) ERR_ALREADY_CLAIMED)
    (asserts! (is-some (get runner-up-entry-id contest)) ERR_NOT_WINNER)
    (asserts! (is-eq (get runner-up-entry-id contest) (some (get entry-id user-entry))) ERR_NOT_WINNER)
    (map-set prize-claims
      { contest-id: contest-id, participant: tx-sender }
      { claimed: true, amount: runner-up-amount })
    (as-contract (stx-transfer? runner-up-amount tx-sender tx-sender))))

(define-public (claim-creator-fee (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR_CONTEST_NOT_FOUND))
        (prize-pool (get prize-pool contest))
        (winner-amount (/ (* prize-pool (get winner-percentage contest)) u100))
        (runner-up-amount (/ (* prize-pool (get runner-up-percentage contest)) u100))
        (creator-fee (- prize-pool (+ winner-amount runner-up-amount))))
    (asserts! (is-eq tx-sender (get creator contest)) ERR_UNAUTHORIZED)
    (asserts! (get is-finalized contest) ERR_CONTEST_NOT_ENDED)
    (if (> creator-fee u0)
      (as-contract (stx-transfer? creator-fee tx-sender (get creator contest)))
      (ok true))))

(define-public (emergency-withdraw (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) ERR_CONTEST_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator contest)) ERR_UNAUTHORIZED)
    (asserts! (> stacks-block-height (+ (get end-block contest) u1440)) ERR_CONTEST_NOT_ENDED)
    (asserts! (not (get is-finalized contest)) ERR_ALREADY_CLAIMED)
    (as-contract (stx-transfer? (get prize-pool contest) tx-sender (get creator contest)))))

(define-read-only (get-contest (contest-id uint))
  (map-get? contests { contest-id: contest-id }))

(define-read-only (get-entry (entry-id uint))
  (map-get? entries { entry-id: entry-id }))

(define-read-only (get-vote (contest-id uint) (voter principal))
  (map-get? votes { contest-id: contest-id, voter: voter }))

(define-read-only (get-user-entry-info (contest-id uint) (participant principal))
  (map-get? user-entries { contest-id: contest-id, participant: participant }))

(define-read-only (get-current-contest-id)
  (var-get contest-id-nonce))

(define-read-only (get-current-entry-id)
  (var-get entry-id-nonce))

(define-read-only (is-contest-active (contest-id uint))
  (match (map-get? contests { contest-id: contest-id })
    contest (and (>= stacks-block-height (get start-block contest))
                 (< stacks-block-height (get end-block contest)))
    false))

(define-read-only (is-contest-ended (contest-id uint))
  (match (map-get? contests { contest-id: contest-id })
    contest (>= stacks-block-height (get end-block contest))
    false))

(define-read-only (has-voted (contest-id uint) (voter principal))
  (is-some (map-get? votes { contest-id: contest-id, voter: voter })))

(define-read-only (get-contest-winner (contest-id uint))
  (match (map-get? contests { contest-id: contest-id })
    contest (ok (get winner-entry-id contest))
    (err u404)))

(define-read-only (get-contest-runner-up (contest-id uint))
  (match (map-get? contests { contest-id: contest-id })
    contest (ok (get runner-up-entry-id contest))
    (err u404)))

(define-read-only (get-prize-info (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) (err u404)))
        (prize-pool (get prize-pool contest))
        (winner-amount (/ (* prize-pool (get winner-percentage contest)) u100))
        (runner-up-amount (/ (* prize-pool (get runner-up-percentage contest)) u100)))
    (ok {
      total-pool: prize-pool,
      winner-prize: winner-amount,
      runner-up-prize: runner-up-amount,
      creator-fee: (- prize-pool (+ winner-amount runner-up-amount))
    })))

(define-read-only (get-contest-stats (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) (err u404))))
    (ok {
      title: (get title contest),
      category: (get category contest),
      creator: (get creator contest),
      total-entries: (get total-entries contest),
      prize-pool: (get prize-pool contest),
      is-active: (is-contest-active contest-id),
      is-ended: (is-contest-ended contest-id),
      is-finalized: (get is-finalized contest),
      blocks-remaining: (if (< stacks-block-height (get end-block contest))
                         (- (get end-block contest) stacks-block-height)
                         u0),
      winner: (get winner-entry-id contest),
      runner-up: (get runner-up-entry-id contest)
    })))

(define-read-only (get-prize-claim-status (contest-id uint) (participant principal))
  (map-get? prize-claims { contest-id: contest-id, participant: participant }))

(define-read-only (calculate-contest-fees (entry-fee uint) (expected-entries uint))
  (let ((total-collection (* entry-fee expected-entries))
        (platform-fee (/ total-collection u20)))
    (ok {
      total-collection: total-collection,
      platform-fee: platform-fee,
      available-prizes: (- total-collection platform-fee)
    })))

(define-read-only (get-user-participation (contest-id uint) (participant principal))
  (let ((user-entry (map-get? user-entries { contest-id: contest-id, participant: participant }))
        (user-vote (map-get? votes { contest-id: contest-id, voter: participant })))
    (ok {
      has-entry: (is-some user-entry),
      entry-id: (if (is-some user-entry) (some (get entry-id (unwrap-panic user-entry))) none),
      has-voted: (is-some user-vote),
      voted-for: (if (is-some user-vote) (some (get entry-id (unwrap-panic user-vote))) none)
    })))

(define-read-only (is-winner (contest-id uint) (participant principal))
  (match (map-get? contests { contest-id: contest-id })
    contest (match (map-get? user-entries { contest-id: contest-id, participant: participant })
      user-entry (and (get is-finalized contest)
                      (is-eq (get winner-entry-id contest) (some (get entry-id user-entry))))
      false)
    false))

(define-read-only (is-runner-up (contest-id uint) (participant principal))
  (match (map-get? contests { contest-id: contest-id })
    contest (match (map-get? user-entries { contest-id: contest-id, participant: participant })
      user-entry (and (get is-finalized contest)
                      (is-eq (get runner-up-entry-id contest) (some (get entry-id user-entry))))
      false)
    false))

(define-read-only (get-contest-timeline (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) (err u404))))
    (ok {
      start-block: (get start-block contest),
      end-block: (get end-block contest),
      current-block: stacks-block-height,
      blocks-until-start: (if (< stacks-block-height (get start-block contest))
                           (- (get start-block contest) stacks-block-height)
                           u0),
      blocks-until-end: (if (< stacks-block-height (get end-block contest))
                         (- (get end-block contest) stacks-block-height)
                         u0)
    })))

(define-read-only (get-entry-performance (entry-id uint))
  (let ((entry (unwrap! (map-get? entries { entry-id: entry-id }) (err u404))))
    (ok {
      entry-id: entry-id,
      participant: (get participant entry),
      title: (get title entry),
      votes: (get votes entry),
      submission-block: (get submission-block entry),
      age-in-blocks: (- stacks-block-height (get submission-block entry))
    })))

(define-read-only (validate-contest-setup (winner-percentage uint) (runner-up-percentage uint))
  (ok {
    total-percentage: (+ winner-percentage runner-up-percentage),
    is-valid: (<= (+ winner-percentage runner-up-percentage) u100),
    creator-percentage: (- u100 (+ winner-percentage runner-up-percentage))
  }))

(define-read-only (estimate-prize-distribution (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) (err u404)))
        (current-pool (get prize-pool contest))
        (winner-amount (/ (* current-pool (get winner-percentage contest)) u100))
        (runner-up-amount (/ (* current-pool (get runner-up-percentage contest)) u100))
        (creator-amount (- current-pool (+ winner-amount runner-up-amount))))
    (ok {
      current-prize-pool: current-pool,
      estimated-winner-prize: winner-amount,
      estimated-runner-up-prize: runner-up-amount,
      estimated-creator-fee: creator-amount,
      entry-fee: (get entry-fee contest)
    })))

(define-read-only (get-contest-participation-stats (contest-id uint))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) (err u404))))
    (ok {
      total-entries: (get total-entries contest),
      entry-fee: (get entry-fee contest),
      prize-pool: (get prize-pool contest),
      average-prize-per-entry: (if (> (get total-entries contest) u0)
                                 (/ (get prize-pool contest) (get total-entries contest))
                                 u0)
    })))

(define-read-only (can-claim-prize (contest-id uint) (participant principal))
  (let ((contest (unwrap! (map-get? contests { contest-id: contest-id }) (err u404)))
        (user-entry (map-get? user-entries { contest-id: contest-id, participant: participant }))
        (existing-claim (map-get? prize-claims { contest-id: contest-id, participant: participant })))
    (ok {
      is-finalized: (get is-finalized contest),
      has-entry: (is-some user-entry),
      is-winner: (if (is-some user-entry) 
                   (is-eq (get winner-entry-id contest) (some (get entry-id (unwrap-panic user-entry))))
                   false),
      is-runner-up: (if (is-some user-entry)
                     (is-eq (get runner-up-entry-id contest) (some (get entry-id (unwrap-panic user-entry))))
                     false),
      already-claimed: (is-some existing-claim)
    })))
