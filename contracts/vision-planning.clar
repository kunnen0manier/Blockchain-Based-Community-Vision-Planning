;; ============================================================================
;; Community Vision Planning System
;; ============================================================================
;; A decentralized system for facilitating long-term community development
;; with stakeholder engagement, vision articulation, and implementation tracking

;; ============================================================================
;; CONSTANTS & ERRORS
;; ============================================================================

(define-constant CONTRACT_OWNER tx-sender)

;; Error codes
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_VISION (err u101))
(define-constant ERR_VISION_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_VOTING_CLOSED (err u104))
(define-constant ERR_INVALID_MILESTONE (err u105))
(define-constant ERR_MILESTONE_NOT_FOUND (err u106))
(define-constant ERR_INSUFFICIENT_PARTICIPATION (err u107))
(define-constant ERR_INVALID_STATUS (err u108))

;; Vision statuses
(define-constant STATUS_DRAFT u0)
(define-constant STATUS_VOTING u1)
(define-constant STATUS_APPROVED u2)
(define-constant STATUS_IMPLEMENTATION u3)
(define-constant STATUS_COMPLETED u4)
(define-constant STATUS_REJECTED u5)

;; Milestone statuses
(define-constant MILESTONE_PENDING u0)
(define-constant MILESTONE_IN_PROGRESS u1)
(define-constant MILESTONE_COMPLETED u2)
(define-constant MILESTONE_DELAYED u3)

;; Voting thresholds (percentages * 100 for precision)
(define-constant QUORUM_THRESHOLD u3000) ;; 30%
(define-constant APPROVAL_THRESHOLD u6000) ;; 60%

;; Time constants (in blocks)
(define-constant VOTING_PERIOD u1008) ;; ~1 week (10 min blocks)
(define-constant IMPLEMENTATION_TIMEOUT u14112) ;; ~14 weeks

;; ============================================================================
;; DATA STRUCTURES
;; ============================================================================

;; Community vision structure
(define-map visions
  { vision-id: uint }
  {
    title: (string-ascii 128),
    description: (string-utf8 1024),
    creator: principal,
    created-at: uint,
    status: uint,
    voting-start: (optional uint),
    voting-end: (optional uint),
    votes-for: uint,
    votes-against: uint,
    total-participants: uint,
    implementation-start: (optional uint),
    estimated-completion: (optional uint),
    category: (string-ascii 64),
    priority: uint ;; 1-5 scale
  }
)

;; Stakeholder registration
(define-map stakeholders
  { stakeholder: principal }
  {
    registered-at: uint,
    reputation: uint,
    participation-count: uint,
    last-active: uint,
    role: (string-ascii 32) ;; "resident", "business", "organization", "official"
  }
)

;; Voting records
(define-map votes
  { vision-id: uint, voter: principal }
  {
    vote: bool, ;; true = for, false = against
    voted-at: uint,
    weight: uint ;; based on reputation and role
  }
)

;; Implementation milestones
(define-map milestones
  { vision-id: uint, milestone-id: uint }
  {
    title: (string-ascii 128),
    description: (string-utf8 512),
    target-date: uint,
    status: uint,
    completion-date: (optional uint),
    evidence: (optional (string-utf8 256)), ;; Link to evidence/documentation
    responsible-party: (optional principal)
  }
)

;; Progress tracking
(define-map vision-progress
  { vision-id: uint }
  {
    total-milestones: uint,
    completed-milestones: uint,
    overall-progress: uint, ;; percentage * 100
    last-updated: uint,
    next-review-date: uint
  }
)

;; Comments and feedback
(define-map vision-comments
  { vision-id: uint, comment-id: uint }
  {
    author: principal,
    content: (string-utf8 512),
    created-at: uint,
    parent-comment: (optional uint) ;; For threaded discussions
  }
)

;; ============================================================================
;; DATA VARIABLES
;; ============================================================================

(define-data-var next-vision-id uint u1)
(define-data-var next-comment-id uint u1)
(define-data-var total-stakeholders uint u0)
(define-data-var governance-enabled bool true)

;; ============================================================================
;; STAKEHOLDER MANAGEMENT
;; ============================================================================

;; Register as a stakeholder in the community
(define-public (register-stakeholder (role (string-ascii 32)))
  (let (
    (current-block stacks-block-height)
  )
    (asserts! (is-eq (map-get? stakeholders { stakeholder: tx-sender }) none)
              ERR_NOT_AUTHORIZED)

    (map-set stakeholders
      { stakeholder: tx-sender }
      {
        registered-at: current-block,
        reputation: u100, ;; Starting reputation
        participation-count: u0,
        last-active: current-block,
        role: role
      }
    )

    (var-set total-stakeholders (+ (var-get total-stakeholders) u1))
    (ok true)
  )
)

;; Update stakeholder activity and reputation
(define-private (update-stakeholder-activity (stakeholder principal) (reputation-change int))
  (let (
    (current-stakeholder (unwrap! (map-get? stakeholders { stakeholder: stakeholder })
                                 ERR_NOT_AUTHORIZED))
    (current-block stacks-block-height)
    (new-reputation (if (> reputation-change 0)
                       (+ (get reputation current-stakeholder) (to-uint reputation-change))
                       (if (> (get reputation current-stakeholder) (to-uint (* reputation-change -1)))
                           (- (get reputation current-stakeholder) (to-uint (* reputation-change -1)))
                           u1))) ;; Minimum reputation of 1
  )
    (map-set stakeholders
      { stakeholder: stakeholder }
      (merge current-stakeholder {
        last-active: current-block,
        reputation: new-reputation,
        participation-count: (+ (get participation-count current-stakeholder) u1)
      })
    )
    (ok true)
  )
)

;; ============================================================================
;; VISION CREATION & MANAGEMENT
;; ============================================================================

;; Create a new community vision
(define-public (create-vision
  (title (string-ascii 128))
  (description (string-utf8 1024))
  (category (string-ascii 64))
  (priority uint)
  (estimated-completion-blocks uint))

  (let (
    (vision-id (var-get next-vision-id))
    (current-block stacks-block-height)
  )
    ;; Validate inputs
    (asserts! (> (len title) u0) ERR_INVALID_VISION)
    (asserts! (> (len description) u10) ERR_INVALID_VISION)
    (asserts! (and (>= priority u1) (<= priority u5)) ERR_INVALID_VISION)
    (asserts! (is-some (map-get? stakeholders { stakeholder: tx-sender }))
              ERR_NOT_AUTHORIZED)

    ;; Create the vision
    (map-set visions
      { vision-id: vision-id }
      {
        title: title,
        description: description,
        creator: tx-sender,
        created-at: current-block,
        status: STATUS_DRAFT,
        voting-start: none,
        voting-end: none,
        votes-for: u0,
        votes-against: u0,
        total-participants: u0,
        implementation-start: none,
        estimated-completion: (some (+ current-block estimated-completion-blocks)),
        category: category,
        priority: priority
      }
    )

    ;; Initialize progress tracking
    (map-set vision-progress
      { vision-id: vision-id }
      {
        total-milestones: u0,
        completed-milestones: u0,
        overall-progress: u0,
        last-updated: current-block,
        next-review-date: (+ current-block u2016) ;; ~2 weeks
      }
    )

    ;; Update creator's activity
    (try! (update-stakeholder-activity tx-sender 10))

    (var-set next-vision-id (+ vision-id u1))
    (ok vision-id)
  )
)

;; Start voting on a vision
(define-public (start-voting (vision-id uint))
  (let (
    (vision (unwrap! (map-get? visions { vision-id: vision-id }) ERR_VISION_NOT_FOUND))
    (current-block stacks-block-height)
    (voting-end (+ current-block VOTING_PERIOD))
  )
    ;; Only creator or authorized parties can start voting
    (asserts! (or (is-eq tx-sender (get creator vision))
                  (is-eq tx-sender CONTRACT_OWNER))
              ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status vision) STATUS_DRAFT) ERR_INVALID_STATUS)

    ;; Update vision status
    (map-set visions
      { vision-id: vision-id }
      (merge vision {
        status: STATUS_VOTING,
        voting-start: (some current-block),
        voting-end: (some voting-end)
      })
    )

    (ok true)
  )
)

;; ============================================================================
;; VOTING SYSTEM
;; ============================================================================

;; Cast a vote on a vision
(define-public (vote-on-vision (vision-id uint) (vote bool))
  (let (
    (vision (unwrap! (map-get? visions { vision-id: vision-id }) ERR_VISION_NOT_FOUND))
    (stakeholder (unwrap! (map-get? stakeholders { stakeholder: tx-sender })
                          ERR_NOT_AUTHORIZED))
    (current-block stacks-block-height)
    (vote-weight (calculate-vote-weight stakeholder))
  )
    ;; Validate voting conditions
    (asserts! (is-eq (get status vision) STATUS_VOTING) ERR_VOTING_CLOSED)
    (asserts! (<= current-block (unwrap! (get voting-end vision) ERR_VOTING_CLOSED))
              ERR_VOTING_CLOSED)
    (asserts! (is-eq (map-get? votes { vision-id: vision-id, voter: tx-sender }) none)
              ERR_ALREADY_VOTED)

    ;; Record the vote
    (map-set votes
      { vision-id: vision-id, voter: tx-sender }
      {
        vote: vote,
        voted-at: current-block,
        weight: vote-weight
      }
    )

    ;; Update vision vote counts
    (map-set visions
      { vision-id: vision-id }
      (merge vision {
        votes-for: (if vote
                      (+ (get votes-for vision) vote-weight)
                      (get votes-for vision)),
        votes-against: (if vote
                          (get votes-against vision)
                          (+ (get votes-against vision) vote-weight)),
        total-participants: (+ (get total-participants vision) u1)
      })
    )

    ;; Update voter's activity
    (try! (update-stakeholder-activity tx-sender 5))

    (ok true)
  )
)

;; Calculate vote weight based on stakeholder properties
(define-private (calculate-vote-weight (stakeholder-data (tuple (registered-at uint) (reputation uint) (participation-count uint) (last-active uint) (role (string-ascii 32)))))
  (let (
    (base-weight u100)
    (reputation-bonus (/ (get reputation stakeholder-data) u10))
    (participation-count (get participation-count stakeholder-data))
    (participation-bonus (if (<= participation-count u50) participation-count u50))
    (role-multiplier (if (is-eq (get role stakeholder-data) "official") u150 u100))
  )
    (/ (* (+ base-weight reputation-bonus participation-bonus) role-multiplier) u100)
  )
)

;; Finalize voting and determine outcome
(define-public (finalize-voting (vision-id uint))
  (let (
    (vision (unwrap! (map-get? visions { vision-id: vision-id }) ERR_VISION_NOT_FOUND))
    (current-block stacks-block-height)
    (total-votes (+ (get votes-for vision) (get votes-against vision)))
    (total-stakeholders-count (var-get total-stakeholders))
    (participation-rate (if (> total-stakeholders-count u0)
                           (/ (* (get total-participants vision) u10000) total-stakeholders-count)
                           u0))
    (approval-rate (if (> total-votes u0)
                      (/ (* (get votes-for vision) u10000) total-votes)
                      u0))
  )
    ;; Validate finalization conditions
    (asserts! (is-eq (get status vision) STATUS_VOTING) ERR_INVALID_STATUS)
    (asserts! (>= current-block (unwrap! (get voting-end vision) ERR_VOTING_CLOSED))
              ERR_VOTING_CLOSED)

    ;; Check quorum and approval
    (let (
      (new-status (if (and (>= participation-rate QUORUM_THRESHOLD)
                          (>= approval-rate APPROVAL_THRESHOLD))
                     STATUS_APPROVED
                     STATUS_REJECTED))
    )
      ;; Update vision status
      (map-set visions
        { vision-id: vision-id }
        (merge vision {
          status: new-status,
          implementation-start: (if (is-eq new-status STATUS_APPROVED)
                                   (some current-block)
                                   none)
        })
      )

      ;; Update creator's reputation based on outcome
      (if (is-eq new-status STATUS_APPROVED)
          (try! (update-stakeholder-activity (get creator vision) 20))
          (try! (update-stakeholder-activity (get creator vision) -5))
      )

      (ok new-status)
    )
  )
)

;; ============================================================================
;; MILESTONE MANAGEMENT
;; ============================================================================

;; Add implementation milestone
(define-public (add-milestone
  (vision-id uint)
  (milestone-id uint)
  (title (string-ascii 128))
  (description (string-utf8 512))
  (target-date uint)
  (responsible-party (optional principal)))

  (let (
    (vision (unwrap! (map-get? visions { vision-id: vision-id }) ERR_VISION_NOT_FOUND))
    (progress (unwrap! (map-get? vision-progress { vision-id: vision-id })
                      ERR_VISION_NOT_FOUND))
  )
    ;; Validate authorization and vision status
    (asserts! (or (is-eq tx-sender (get creator vision))
                  (is-eq tx-sender CONTRACT_OWNER))
              ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status vision) STATUS_APPROVED) ERR_INVALID_STATUS)
    (asserts! (is-eq (map-get? milestones { vision-id: vision-id, milestone-id: milestone-id }) none)
              ERR_INVALID_MILESTONE)

    ;; Create milestone
    (map-set milestones
      { vision-id: vision-id, milestone-id: milestone-id }
      {
        title: title,
        description: description,
        target-date: target-date,
        status: MILESTONE_PENDING,
        completion-date: none,
        evidence: none,
        responsible-party: responsible-party
      }
    )

    ;; Update progress tracking
    (map-set vision-progress
      { vision-id: vision-id }
      (merge progress {
        total-milestones: (+ (get total-milestones progress) u1),
        last-updated: stacks-block-height
      })
    )

    (ok true)
  )
)

;; Update milestone status
(define-public (update-milestone-status
  (vision-id uint)
  (milestone-id uint)
  (new-status uint)
  (evidence (optional (string-utf8 256))))

  (let (
    (vision (unwrap! (map-get? visions { vision-id: vision-id }) ERR_VISION_NOT_FOUND))
    (milestone (unwrap! (map-get? milestones { vision-id: vision-id, milestone-id: milestone-id })
                       ERR_MILESTONE_NOT_FOUND))
    (current-block stacks-block-height)
  )
    ;; Validate authorization
    (asserts! (or (is-eq tx-sender (get creator vision))
                  (is-eq tx-sender CONTRACT_OWNER)
                  (is-eq (some tx-sender) (get responsible-party milestone)))
              ERR_NOT_AUTHORIZED)
    (asserts! (<= new-status MILESTONE_DELAYED) ERR_INVALID_STATUS)

    ;; Update milestone
    (map-set milestones
      { vision-id: vision-id, milestone-id: milestone-id }
      (merge milestone {
        status: new-status,
        completion-date: (if (is-eq new-status MILESTONE_COMPLETED)
                            (some current-block)
                            (get completion-date milestone)),
        evidence: evidence
      })
    )

    ;; Update overall progress if milestone completed
    (if (is-eq new-status MILESTONE_COMPLETED)
        (update-vision-progress vision-id)
        (ok true)
    )
  )
)

;; Update overall vision progress
(define-private (update-vision-progress (vision-id uint))
  (let (
    (progress (unwrap! (map-get? vision-progress { vision-id: vision-id })
                      ERR_VISION_NOT_FOUND))
    (new-completed (+ (get completed-milestones progress) u1))
    (total (get total-milestones progress))
    (new-progress (if (> total u0)
                     (/ (* new-completed u10000) total)
                     u0))
  )
    ;; Update progress
    (map-set vision-progress
      { vision-id: vision-id }
      (merge progress {
        completed-milestones: new-completed,
        overall-progress: new-progress,
        last-updated: stacks-block-height,
        next-review-date: (+ stacks-block-height u2016) ;; ~2 weeks
      })
    )

    ;; Check if vision is fully completed
    (if (is-eq new-completed total)
        (let (
          (vision (unwrap! (map-get? visions { vision-id: vision-id }) ERR_VISION_NOT_FOUND))
        )
          (map-set visions
            { vision-id: vision-id }
            (merge vision { status: STATUS_COMPLETED })
          )
          (ok true)
        )
        (ok true)
    )
  )
)

;; ============================================================================
;; COMMUNITY ENGAGEMENT
;; ============================================================================

;; Add comment to a vision
(define-public (add-comment
  (vision-id uint)
  (content (string-utf8 512))
  (parent-comment (optional uint)))

  (let (
    (comment-id (var-get next-comment-id))
  )
    ;; Validate inputs
    (asserts! (is-some (map-get? visions { vision-id: vision-id })) ERR_VISION_NOT_FOUND)
    (asserts! (is-some (map-get? stakeholders { stakeholder: tx-sender }))
              ERR_NOT_AUTHORIZED)
    (asserts! (> (len content) u0) ERR_INVALID_VISION)

    ;; Create comment
    (map-set vision-comments
      { vision-id: vision-id, comment-id: comment-id }
      {
        author: tx-sender,
        content: content,
        created-at: stacks-block-height,
        parent-comment: parent-comment
      }
    )

    ;; Update commenter's activity
    (try! (update-stakeholder-activity tx-sender 2))

    (var-set next-comment-id (+ comment-id u1))
    (ok comment-id)
  )
)

;; ============================================================================
;; READ-ONLY FUNCTIONS
;; ============================================================================

;; Get vision details
(define-read-only (get-vision (vision-id uint))
  (map-get? visions { vision-id: vision-id })
)

;; Get stakeholder info
(define-read-only (get-stakeholder (stakeholder principal))
  (map-get? stakeholders { stakeholder: stakeholder })
)

;; Get vote info
(define-read-only (get-vote (vision-id uint) (voter principal))
  (map-get? votes { vision-id: vision-id, voter: voter })
)

;; Get milestone info
(define-read-only (get-milestone (vision-id uint) (milestone-id uint))
  (map-get? milestones { vision-id: vision-id, milestone-id: milestone-id })
)

;; Get vision progress
(define-read-only (get-vision-progress (vision-id uint))
  (map-get? vision-progress { vision-id: vision-id })
)

;; Get comment
(define-read-only (get-comment (vision-id uint) (comment-id uint))
  (map-get? vision-comments { vision-id: vision-id, comment-id: comment-id })
)

;; Get voting results
(define-read-only (get-voting-results (vision-id uint))
  (let (
    (vision (map-get? visions { vision-id: vision-id }))
  )
    (match vision
      vision-data (some {
        votes-for: (get votes-for vision-data),
        votes-against: (get votes-against vision-data),
        total-participants: (get total-participants vision-data),
        status: (get status vision-data),
        voting-end: (get voting-end vision-data)
      })
      none
    )
  )
)

;; Check if stakeholder can vote
(define-read-only (can-vote (vision-id uint) (stakeholder principal))
  (let (
    (vision (map-get? visions { vision-id: vision-id }))
    (stakeholder-info (map-get? stakeholders { stakeholder: stakeholder }))
    (existing-vote (map-get? votes { vision-id: vision-id, voter: stakeholder }))
  )
    (and
      (is-some vision)
      (is-some stakeholder-info)
      (is-none existing-vote)
      (is-eq (get status (unwrap-panic vision)) STATUS_VOTING)
      (<= stacks-block-height (unwrap-panic (get voting-end (unwrap-panic vision))))
    )
  )
)

;; Get system stats
(define-read-only (get-system-stats)
  {
    total-visions: (- (var-get next-vision-id) u1),
    total-stakeholders: (var-get total-stakeholders),
    total-comments: (- (var-get next-comment-id) u1),
    governance-enabled: (var-get governance-enabled)
  }
)
