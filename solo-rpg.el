;;; solo-rpg.el --- Solo roleplaying games support functions  -*- lexical-binding: t; -*-

;; Author: Christer Enfors <christer.enfors@gmail.com>
;; Maintainer: Christer Enfors <christer.enfors@gmail.com>
;; Created: 2026
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games, convenience, wp
;; URL: https://github.com/enfors/solo-rpg

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Solo-rpg is a set up support functions for playing solo roleplaying games
;; in Emacs.

;; Features include:
;; - Dice rolling
;; - Oracles:
;;   - Yes/No Oracle with probabilities
;;   - Action/Theme Oracle, inspired by the Mything Game Master Emulator
;;
;; To use this package, add the following to your configuration:
;;
;;   (require 'solo-rpg)
;;   (with-eval-after-load 'solo-rpg
;;     ;; Note - you can replace "C-c r" with another key if you prefer
;;     (define-key solo-rpg-mode-map (kbd "C-c r") 'solo-rpg-menu)
;;
;; To start solo-rpg-mode, type:
;;
;;   M-x solo-rpg-mode
;;
;; Once solo-rpg-mode is started, you can invoke the dashboard menu by
;; typing the shortcut key you chose above ("C-c r" by default).
;;
;; Customization:
;;
;;   `solo-rpg-output-method' can be set to 'insert or 'message.
;;      - If set to 'message, output will be sent to message buffer only.
;;      - If set to 'insert, output will also be inserted in current buffer.


;;; Code:

(require 'cl-lib)  ;; For structs
(require 'transient)

;;; Configuration variables:

(defcustom solo-rpg-output-method 'insert
  "Default method for outputting solo-rpg results.
Can be `insert' to put text in current buffer, or `message' to only echo it."
  :type '(choice (const :tag "Insert in current buffer" insert)
                 (const :tag "Only show in message area" message))
  :group 'solo-rpg)


;;; Tables:

(defconst solo-rpg-oracle-actions
  ["Abandon"
   "Accept"
   "Accuse"
   "Ambush"
   "Assault"
   "Assist"
   "Attack"
   "Avoid"
   "Balance"
   "Begin"
   "Believe"
   "Betray"
   "Beware"
   "Break"
   "Build"
   "Burn"
   "Cancel"
   "Collect"
   "Collide"
   "Compete"
   "Create"
   "Damage"
   "Deal"
   "Deceive"
   "Decide"
   "Defeat"
   "Defend"
   "Demolish"
   "Deny"
   "Destroy"
   "Detect"
   "Determine"
   "Disable"
   "Dominate"
   "Elaborate"
   "Eliminate"
   "Emerge"
   "Empower"
   "Endanger"
   "Engage"
   "Enhance"
   "Erase"
   "Escape"
   "Fabricate"
   "Fail"
   "Fear"
   "Fight"
   "Flee"
   "Follow"
   "Forget"
   "Fortify"
   "Gain"
   "Grow"
   "Halt"
   "Heal"
   "Hide"
   "Hinder"
   "Hurt"
   "Impersonate"
   "Implicate"
   "Interrupt"
   "Investigate"
   "Keep"
   "Lead"
   "Learn"
   "Leverage"
   "Locate"
   "Mediate"
   "Mislead"
   "Negate"
   "Obey"
   "Observe"
   "Oppress"
   "Promise"
   "Protect"
   "Pursue"
   "Raid"
   "Raise"
   "Recover"
   "Reject"
   "Relocate"
   "Remove"
   "Restrict"
   "Reveal"
   "Ruin"
   "Sabotage"
   "Save"
   "Search"
   "Stop"
   "Talk"
   "Tempt"
   "Terminate"
   "Value"
   "Venture"
   "Verify"
   "Vilify"
   "Violate"
   "Warn"
   "Weaken"
   "Withdraw"]
  "A d100 table of Action words for the Action/Theme oracle.")

(defconst solo-rpg-oracle-themes
  ["Allegations"
   "Alliances"
   "Allies"
   "Ambition"
   "Anger"
   "Artifact"
   "Beginnings"
   "Betrayal"
   "Bonds"
   "Border"
   "Child"
   "Community"
   "Consent"
   "Curse"
   "Darkness"
   "Desecration"
   "Desolation"
   "Devastation"
   "Devotion"
   "Disaster"
   "Discovery"
   "Doom"
   "Dreams"
   "Emotions"
   "Enemies"
   "Fear"
   "Forest"
   "Greatness"
   "Happiness"
   "Hate"
   "Hidden, the"
   "Homes"
   "Hope"
   "Illusion"
   "Innocent"
   "Intrigue"
   "Joy"
   "Kind"
   "Kingdom"
   "Knowledge"
   "Landscape"
   "Language"
   "Leader"
   "Leadership"
   "Legend"
   "Liberty"
   "Lies"
   "Light"
   "Limitations"
   "Loan"
   "Lock"
   "Lord"
   "Lore"
   "Love"
   "Machine"
   "Madness"
   "Magic"
   "Malice"
   "Mania"
   "Master"
   "Medicine"
   "Monsters"
   "Nature"
   "Neglect"
   "Night"
   "Nightmare"
   "Obligation"
   "Oblivion"
   "Occupant"
   "Offense"
   "Opportunity"
   "Opposition"
   "Oppression"
   "Passion"
   "Peace"
   "Plague"
   "Plans"
   "Political, the"
   "Possibilities"
   "Power"
   "Prophet"
   "Quarrel"
   "Realm"
   "Rejection"
   "Reward"
   "Sadness"
   "Science"
   "Secrets"
   "Technology"
   "Terrain"
   "Threat"
   "Traitor"
   "Treason"
   "Truth"
   "Unity"
   "Values"
   "Vandalism"
   "Vicinity"
   "Vision"
   "War"]
  "A d100 table of Theme words for the Action/Theme oracle.")

;;; Yes/No oracle tables

(defconst solo-rpg-oracle-yes-no-table
  '(("+6 Almost certainly"      . (2  3  4  7 16))
    ("+5 Highly likely"         . (2  4  5  8 16))
    ("+4 Very likely"           . (2  5  6  9 17))
    ("+3 Likely"                . (2  6  7  9 17))
    ("+2 Probably"              . (2  6  8 10 17))
    ("+1 Somewhat likely"       . (3  7  9 11 17))
    ("50/50"                    . (3  8 10 12 17))
    ("-1 Somewhat unlikely"     . (3  9 11 13 17))
    ("-2 Probably not"          . (3 10 12 14 18))
    ("-3 Unlikely"              . (3 11 13 14 18))
    ("-4 Very unlikely"         . (3 11 14 15 18))
    ("-5 Highly unlikely"       . (4 12 15 16 18))
    ("-6 Almost certainly not"  . (4 13 16 17 18)))
  "Data table for the Yes/No oracle.
Value is upper thresholds for NoAnd, No, NoBut, YesBut, Yes.")

(defconst solo-rpg-oracle-yes-no-event-subject-table
  ["PC" "NPC" "Faction" "Plot"]
  "Table for random event subjects.")

;;; Quantity oracle table

(defconst solo-rpg-oracle-quantity-table
  '(("Minimum"     .  1)
    ("Much less"   .  3)
    ("Less"        .  7)
    ("As expected" . 13)
    ("More"        . 17)
    ("Much more"   . 19)
    ("Maximum"     . 20))
  "Data table for the Quantity oracle.
Values are upper threshold for each entry.")

;;; Generator tables ======================================================

(defconst solo-rpg-generator-plot-goal-table
  ["Acquire"
   "Destroy"
   "Locate"
   "Escape"
   "Follow"
   "Hide"
   "Restore"
   "Explore"
   "Infiltrate"
   "Prevent"]
  "Goal data table for the Plot generator.")

(defconst solo-rpg-generator-plot-focus-table
  ["Artifact"
   "Relative"
   "PC"
   "Existing NPC"
   "Local ruler"
   "Idea"
   "Revenge"
   "Reward"
   "Love"
   "Material riches"
   "Enlightenment"
   "Information"
   "Knowledge"
   "Location"
   "Mysterious beast"
   "Fabled creature"
   "Well-known beast"
   "Monsters"
   "Enemies"
   "Building or structure"]
  "Focus data table for the Plot generator.")

(defconst solo-rpg-generator-plot-obstacle-table
  ["Lack of resources"
   "Lack of information"
   "Lack of knowledge"
   "Duty"
   "Health"
   "Love"
   "Animosity"
   "Time"
   "Honor"
   "Mysterious circumstances"]
  "Obstacle data table for the Plot generator.")

;;; NPC Generator tables:

;;; Appearance:

(defconst solo-rpg-table-npc-height
  '(("Very short"     .  1)
    ("Short"          .  3)
    ("Somewhat short" .  7)
    ("Average"        . 13)
    ("Somewhat tall"  . 17)
    ("Tall"           . 19)
    ("Very tall"      . 20))
  "Height data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-size
  '(("Very small"     .  1)
    ("Small"          .  3)
    ("Somewhat small" .  7)
    ("Average"        . 13)
    ("Somewhat large" . 17)
    ("Large"          . 19)
    ("Very large"     . 20))
  "Size data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-eye-color
  '(("Light blue" .  2)
    ("Blue"       .  4)
    ("Grey"       .  5)
    ("Brown"      .  7)
    ("Dark brown" .  9)
    ("Green"      . 10))
  "Eye color data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-skin-color
  '(("Western" . 5)
    ("African" . 7)
    ("Asian"   . 8))
  "Skin color data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-hair-color
  '(("Blonde" .  2)
    ("Brown"  .  5)
    ("Auburn" .  7)
    ("Red"    .  8)
    ("Dark"   . 10))
  "Hair color data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-hair-length
  '(("Short"           . 2)
    ("Shoulder length" . 3)
    ("Long"            . 5)
    ("Very long"       . 6))
  "Hair length data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-long-hair-style
  '(("Loose"              .  2)
    ("Pony tail"          .  4)
    ("Bun"                .  5)
    ("Braided"            .  7)
    ("Half-up, half-down" .  9)
    ("Dreadlocks"         . 10))
  "Long hair style data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-facial-hair
  '(("None"         .  5)
    ("Beard"        .  7)
    ("Mustache"     .  8)
    ("Sideburns"    . 10)
    ("Mutton chops" . 11)
    ("Goatee"       . 12))
  "Facial hair data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-special-features
  '["Facial scar"
    "Facial birth mark"
    "Piercings"
    "Tattoos"
    "Prominent nose"
    "Distinctive eyebrows"
    "Freckles"
    "Thin lips"
    "Full lips"
    "High cheekbones"
    "Round face"
    "Piercing gaze"
    "Wide nose"
    "Protruding ears"
    "Cleft chin"
    "Deep dimples"
    "Pockmarked skin"
    "Square jaw"
    "Missing tooth"
    "Broken/misshaped nose"]
  "Special features data table for the NPC Appearance generator.")


;;; Other variables:

(defvar solo-rpg--last-dice-string "2d6"
  "The last dice string rolled by the user. Used in the default prompt.")

(defvar solo-rpg--return-buffer nil
  "Remembers which buffer to insert the final text into.")

(defvar solo-rpg--staging-buffer-name "*solo-rpg-staging*"
  "The name of our temporary staging area buffer.")

(defvar solo-rpg--generate-fun nil
  "The callback function which returns generated text for staging.")


;;; Utility functions:

(defun solo-rpg--output (text &optional invert-behavior)
  "Output TEXT according to `solo-rpg-output-method'.
If INVERT-BEHAVIOR is non-nil, do the opposite of the default setting."
  ;; Always log it to the message area.
  (message "%s" text)

  ;; Determine if we should insert based on setting and override
  (let ((should-insert (eq solo-rpg-output-method 'insert)))
    (when invert-behavior
      (setq should-insert (not should-insert)))

    ;; Insert if needed
    (when should-insert
      (solo-rpg--space-insert text))))

(defun solo-rpg--space-insert (&rest args)
  "Like `insert' but prepend a space if preceding character isn't whitespace.
Then, ARGS is printed."
  ;; Check if we need a space.
  ;; We ensure that we aren't at the beginning of the buffer (nil),
  ;; and that the previous char isn't a space, newline, or tab.
  (when (and (char-before)
             (not (memq (char-before) '(?\s ?\n ?\t))))
    (insert " "))
  ;; Pass all arguments directly to the standard insert function.
  (apply #'insert args))


;;; Staging functions:

(defun solo-rpg--stage (generate-fun)
  "The main API to stage generated content.
GENERATE-FUN is a function pointer to function which returns generated text."
  ;; Save the current buffer.
  (setq solo-rpg--return-buffer (current-buffer))

  ;; Save the function pointer
  (setq solo-rpg--generate-fun generate-fun)

  ;; Generate content
  (solo-rpg-staging-regenerate)
  
  ;; Go to the menu
  (solo-rpg-menu-staging))

(defun solo-rpg--staging-update (text)
  "Wipe the staging buffer, insert TEXT, and show it at the bottom."
  (with-current-buffer (get-buffer-create solo-rpg--staging-buffer-name)
    (erase-buffer)
    (insert text)
    ;; Show it at bottom, taking up ~20% of the height
    (display-buffer (current-buffer)
                    '(display-buffer-at-bottom . ((window-height . 0.2))))))

;; Temporary function for testing generation
(defun solo-rpg-staging-regenerate ()
  "Call the `solo-rpg--generate-fun' to generate text, send it to staging."
  (interactive)
  (solo-rpg--staging-update (funcall solo-rpg--generate-fun)))

(defun solo-rpg-staging-keep ()
  "Grab the text, insert it into the original buffer, and clean up."
  (interactive)
  (let ((final-text (with-current-buffer solo-rpg--staging-buffer-name
                      (buffer-string))))
    ;; Clean up the staging window and kill the temporary buffer
    (let ((win (get-buffer-window solo-rpg--staging-buffer-name)))
      (when win (delete-window win)))
    (kill-buffer solo-rpg--staging-buffer-name)

    ;; Go back to original buffer and insert it
    (when (buffer-live-p solo-rpg--return-buffer)
      (with-current-buffer solo-rpg--return-buffer
        (solo-rpg--output final-text)))))

(defun solo-rpg-staging-abort ()
  "Close the staging area without doing anything."
  (interactive)
  (let ((win (get-buffer-window solo-rpg--staging-buffer-name)))
    (when win (delete-window win)))
  (kill-buffer solo-rpg--staging-buffer-name))

;; Staging menu
(transient-define-prefix solo-rpg-menu-staging ()
  ["Staging Area Options"
   ;; Note the :transient t here! This keeps the menu open after pressing 'r'
   ("r" "Regenerate"    solo-rpg-staging-regenerate :transient t)
   ("k" "Keep (insert)" solo-rpg-staging-keep)
   ("q" "Abort"         solo-rpg-staging-abort)])

;; The entry point
;; (defun solo-rpg-staging-hello-world ()
;;   "Launch the staging area test."
;;   (interactive)
;;   ;; Step 1: Remember where we area
;;   (setq solo-rpg--return-buffer (current-buffer))
;;   ;; Step 2: Genereate the very first iteration
;;   (solo-rpg-staging-regenerate)
;;   ;; Step 3: Summon the transient menu to wait for the user
;;   (solo-rpg-menu-staging))


;;; Table functions:

(defun solo-rpg-table-get-random (table)
  "Return random element from TABLE."
  (aref table (random (length table))))

(defun solo-rpg--table-weighted-get-random (weighted-table max-value)
  "Return random element from WEIGHTED-TABLE where max value is MAX-VALUE."
  (let ((roll (+ 1 (random max-value))))
    (cl-loop for cell in weighted-table
             for label = (car cell)
             for threshold = (cdr cell)
             if (<= roll threshold)
             ;;             return (format "[%d] -> %s " roll label))))
             return label)))


;;; Dashboard functions:

(defun solo-rpg-output-method-toggle ()
  "Toggle `solo-rpg-output-method' between `insert' and `message'."
  (interactive)
  (if (eq solo-rpg-output-method 'insert)
      (setq solo-rpg-output-method 'message)
    (setq solo-rpg-output-method 'insert))
  (message "Solo-RPG output method is now: %s" solo-rpg-output-method))

(defun solo-rpg--toggle-output-desc ()
  "Return a formatted string showing the current output method."
  (format "Toggle output (currently: %s)" solo-rpg-output-method))

;;; Dice rolls:

(cl-defstruct solo-rpg-dice-roll
  count    ;; Number of dice
  sides    ;; Die sizes
  mod      ;; Modifier (+/-)
  rolls    ;; List of each die roll result
  total)   ;; Sum of all rolls + mod

(defun solo-rpg-dice-string-parse (dice-string)
  "Ask for DICE-STRING and parse it, returning a solo-rpg-dice-roll struct."
  (unless (string-match "^\\([0-9]+\\)*d\\([0-9]+\\)*\\([+-][0-9]+\\)?"
                        dice-string)
    (user-error "Invalid dice format '%s'. Try '2d6' or '1d20+5'"
                dice-string))
  (let ((count (string-to-number (or (match-string 1 dice-string) "1")))
        (sides (string-to-number (or (match-string 2 dice-string) "6")))
        (mod   (string-to-number (or (match-string 3 dice-string) "0"))))
    ;; Return the struct
    (make-solo-rpg-dice-roll
     :count count
     :sides sides
     :mod mod
     :rolls nil                      ; No rolls have been made yet
     :total nil)))                   ; No totals, because no rolls yet
    
(defun solo-rpg-dice-roll-calculate (roll-struct)
    "Roll the dice in the specified ROLL-STRUCT and fill in the results.
Returns the updated struct."

  ;; Get the data from the struct.
  (let* ((count (solo-rpg-dice-roll-count roll-struct))
         (sides (solo-rpg-dice-roll-sides roll-struct))
         (mod   (solo-rpg-dice-roll-mod   roll-struct))

         ;; Generate random numbers. We use cl-loop to repeat the action
         ;; 'count' times (count = number of dice)
         (results (cl-loop repeat count
                           ;; (random sides returns 0 to sides-1.
                           ;; So we add 1.
                           ;; "collect" is a keyword for the cl-loop macro which
                           ;; builds a list.
                           ;; cl-loop is like a whole language of its own.
                           collect (+ 1 (random sides))))
         ;; Calculate the grand total.
         ;; Sum the results and add the modifier.
         (sum (apply #'+ results))      ; #' is used to quote functions
                                        ; apply is used to step through results
         (grand-total (+ sum mod)))

    ;; Update the struct slots with 'setf' - "set field"
    (setf (solo-rpg-dice-roll-rolls roll-struct) results)
    (setf (solo-rpg-dice-roll-total roll-struct) grand-total)

    ;; Return the updated struct
    roll-struct))

(defun solo-rpg-dice-roll-string (roll-struct)
  "Return a string of the ROLL-STRUCT, meant for displaying to the user."
  (let* ((mod-string (if (not (= 0 (solo-rpg-dice-roll-mod roll-struct)))
                         (format "%+d" (solo-rpg-dice-roll-mod roll-struct))
                       ""))
         (rolls-string (mapconcat (lambda (x) (format "[%d]" x))
                      (solo-rpg-dice-roll-rolls roll-struct)
                      "+")))
    (format "%dd%d%s = %s%s = %d"
            (solo-rpg-dice-roll-count roll-struct)
            (solo-rpg-dice-roll-sides roll-struct)
            mod-string
            rolls-string
            mod-string
            (solo-rpg-dice-roll-total roll-struct))))

(defun solo-rpg-dice-roll-cast (dice-string &optional invert)
  "Take DICE-STRING, parse it, make the roll, and output the result.
If INVERT is non-nil, then output mode is inverted."
  (interactive
   ;; Create a temporary scope to build the prompt and ask the user
   (let* ((prompt (format "Enter dice to roll (default: %s): "
                          solo-rpg--last-dice-string))
          (input (read-string prompt nil nil solo-rpg--last-dice-string)))
     ;; Return the list of arguments to hand to the function
     (list input current-prefix-arg)))

  ;; Save the string for next time
  (setq solo-rpg--last-dice-string dice-string)
  
  (solo-rpg--output
   (solo-rpg-dice-roll-string (solo-rpg-dice-roll-calculate (solo-rpg-dice-string-parse
                                                             dice-string))) invert))

;; (defun solo-rpg-dice-roll-message (dice-string)
;;   "Ask for DICE-STRING, roll the dice, display result in message window."
;;   (interactive "sEnter dice string (for example '2d6+2'): ")
;;   (message (solo-rpg-dice-roll-cast dice-string)))

;; (defun solo-rpg-dice-roll-insert (dice-string)
;;   "Ask for DICE-STRING, roll the dice, output the result in the current buffer."
;;   (interactive "sEnter dice string (for example '2d6+2'): ")
;;   (insert (solo-rpg-dice-roll-cast dice-string)))


;;; Action / Theme oracle:

(defun solo-rpg-oracle-action-theme (&optional invert)
  "Output (action) / (theme) where action and theme are random.
If INVERT is non-nil, then output mode is inverted."
  (interactive "P")
  (solo-rpg--output (format "%s / %s"
                            (solo-rpg-table-get-random solo-rpg-oracle-actions)
                            (solo-rpg-table-get-random solo-rpg-oracle-themes))
                    invert))

;;; Yes / No oracle:

(defun solo-rpg-oracle-yes-no-string (odds-string)
  "Return NoAnd, No, NoBut, YesBut, Yes, or YesAnd based on ODDS-STRING."
  (let* ((thresholds (cdr (assoc odds-string solo-rpg-oracle-yes-no-table)))
         (roll (+ 1 (random 20)))
         (result-text nil))
    (setq result-text
          (cond
           ;; Handle random events
           ((= roll 1)
            (format "Random event (negative) related to %s."
                    (solo-rpg-table-get-random solo-rpg-oracle-yes-no-event-subject-table)))
           ((= roll 20)
            (format "Random event (positive) related to %s."
                    (solo-rpg-table-get-random solo-rpg-oracle-yes-no-event-subject-table)))
           ;; Handle ordinary lookups
           ((<= roll (nth 0 thresholds)) "No, and...")
           ((<= roll (nth 1 thresholds)) "No")
           ((<= roll (nth 2 thresholds)) "No, but...")
           ((<= roll (nth 3 thresholds)) "Yes, but...")
           ((<= roll (nth 4 thresholds)) "Yes")
           (t "Yes, and...")))
    ;; Return formatted string.
    ;; We use 'car' on split-string to just show "+3" instead of the full text.
    (format "%s: [%d] -> %s"
            odds-string
            roll
            result-text)))

(defun solo-rpg-oracle-yes-no (odds &optional invert)
  "Query the Yes/No oracle.
ODDS is the probability string selected from `solo-rpg-oracle-yes-no-table'.
If INVERT is non-nil, then output is inverted."
  (interactive
   (list (completing-read "Probability (Default 50/50): "
                          solo-rpg-oracle-yes-no-table
                          nil t nil nil
                          "50/50")
         current-prefix-arg))
  
  (let ((result (solo-rpg-oracle-yes-no-string odds)))
    (solo-rpg--output result invert)))


;;; Quantity oracle:

(defun solo-rpg-oracle-quantity (&optional invert)
  "Query the Quantity oracle, displaying the result.
If INVERT is non-nil, then output is inverted."
  (interactive "P")
  (solo-rpg--output (solo-rpg--table-weighted-get-random solo-rpg-oracle-quantity-table 20) invert))


;;; Generators:

;;; Plot generator:

(defun solo-rpg--generator-plot-text ()
  "Generate and return a plot text."
  (format "%s %s, but %s"
           (solo-rpg-table-get-random solo-rpg-generator-plot-goal-table)
           (solo-rpg-table-get-random solo-rpg-generator-plot-focus-table)
           (solo-rpg-table-get-random solo-rpg-generator-plot-obstacle-table)))

(defun solo-rpg-generator-plot ()
  "Generate a plot and open it in the staging area."
  (interactive)
  (solo-rpg--stage #'solo-rpg--generator-plot-text))

;;; NPC Appearance generator:

(defun solo-rpg--generator-npc-appearance-text ()
  "Generate and return NPC Appearance text."
  (format (concat "Height          : %s\n"
                  "Size            : %s\n"
                  "Eye color       : %s\n"
                  "Skin color      : %s\n"
                  "Hair            : %s\n"
                  "Facial hair     : %s\n"
                  "Special features: %s\n"
                  )
          (solo-rpg--table-weighted-get-random solo-rpg-table-npc-height 20)
          (solo-rpg--table-weighted-get-random solo-rpg-table-npc-size   20)
          (solo-rpg--table-weighted-get-random solo-rpg-table-npc-eye-color 10)
          (solo-rpg--table-weighted-get-random solo-rpg-table-npc-skin-color 8)
          (solo-rpg--table-weighted-get-random solo-rpg-table-npc-hair-color 10)
          (solo-rpg--table-weighted-get-random
           solo-rpg-table-npc-facial-hair 12)
          (solo-rpg-table-get-random solo-rpg-table-npc-special-features)))

(defun solo-rpg-generator-npc-appearance ()
  "Generate NPC appearance and open it in the staging area."
  (interactive)
  (solo-rpg--stage #'solo-rpg--generator-npc-appearance-text))


;;; Transient dashboard

(transient-define-prefix solo-rpg-menu-dice ()
  "The solo-rpg Dice menu."
  ["Solo-PRG dashboard: Dice Menu"
   ["Actions"
    ("r" "Roll dice"   solo-rpg-dice-roll-cast)
    ("q" "Go back"     transient-quit-one)]])

;;; Oracle dashboards

(transient-define-prefix solo-rpg-menu-oracle-yes-no-probable ()
  "The solo-rpg menu for the Yes/No Oracle with better than 50/50 probability."
  ["Probability"
   ("6" "Almost certainly"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "+6 Almost certainly" invert)))
   ("5" "Highly likely"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "+5 Highly likely" invert)))
   ("4" "Very likely"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "+4 Very likely" invert)))
   ("3" "Likely"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "+3 Likely" invert)))
   ("2" "Probably"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "+2 Probably" invert)))
   ("1" "Somewhat likely"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "+1 Somewhat likely" invert)))
   ("q" "Go back" transient-quit-one)])

(transient-define-prefix solo-rpg-menu-oracle-yes-no-unprobable ()
  "The solo-rpg menu for the Yes/No Oracle with worse than 50/50 probability."
  ["Probability"
   ("1" "Somewhat unlikely"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "-1 Somewhat unlikely" invert)))
   ("2" "Probably not"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "-2 Probably not" invert)))
   ("3" "Unlikely"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "-3 Unlikely" invert)))
   ("4" "Very unlikely"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "-4 Very unlikely" invert)))
   ("5" "Highly unlikely"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "-5 Highly unlikely" invert)))
   ("6" "Almost certainly not"
    (lambda (&optional invert) (interactive "P")
      (solo-rpg-oracle-yes-no "-6 Almost certainly not" invert)))
   ("q" "Go back" transient-quit-one)])

;; Define the Oracle dashboard menu

(transient-define-prefix solo-rpg-menu-oracle ()
  "The solo-rpg Oracle menu."
  ["Actions"
   ("a" "Action/Theme Oracle"   solo-rpg-oracle-action-theme)
   ("u" "Quantity Oracle"       solo-rpg-oracle-quantity)
   ("q" "Go back"               transient-quit-one)]
  ["Yes/No Oracle"
   ("+" "Better than 50/50 probability" solo-rpg-menu-oracle-yes-no-probable)
   ("0" "50/50 probability"
    (lambda (&optional invert) (interactive)
      (solo-rpg-oracle-yes-no "50/50" invert)))
   ("-" "Worse than 50/50 probability"   solo-rpg-menu-oracle-yes-no-unprobable)])

;; Define the Generator dashboard menu

(transient-define-prefix solo-rpg-menu-generator ()
  "The solo-rpg Generator menu."
  ["SoloRPG dashboard: Generator Menu\n"
   ["Actions"
    ("p" "Plot generator"        solo-rpg-generator-plot)
    ("q" "Go back"               transient-quit-one)]
   ["NPCs"
    ("a" "NPC appearance"        solo-rpg-generator-npc-appearance)]])

;; Define the main dashboard menu
(transient-define-prefix solo-rpg-menu ()
  "The main solo-rpg menu."
  ["Solo-RPG dashboard: Main Menu\n"
   ["Modules"
    ("d" "Dice..."       solo-rpg-menu-dice)
    ("o" "Oracles..."    solo-rpg-menu-oracle)
    ("g" "Generators..." solo-rpg-menu-generator)]
   ["System"
    ("t" solo-rpg-output-method-toggle
     :description solo-rpg--toggle-output-desc
     :transient t)
    ("q" "Quit"          transient-quit-one)]])


;;; Minor mode:

;;;###autoload
(defvar solo-rpg-mode-map (make-sparse-keymap)
  "Keymap for `solo-rpg-mode'.
By default, this is empty to allow users to define their own menu key.")

;;;###autoload
(define-minor-mode solo-rpg-mode
  "Minor mode with tools for playing solo roleplaying games.

\\{solo-rpg-mode-map}"
  :init-value nil
  :global nil
  :group 'solo-rpg
  :lighter " SoloRPG"
  :keymap solo-rpg-mode-map

  (if solo-rpg-mode
      ;; If ON:
      (message "Solo-RPG-mode enabled.")
    (message "Solo-RPG-mode disabled.")))

(provide 'solo-rpg)

;;; solo-rpg.el ends here
