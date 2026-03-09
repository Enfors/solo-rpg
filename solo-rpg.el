;;; solo-rpg.el --- Solo roleplaying games support functions  -*- lexical-binding: t; -*-

;; Author: Christer Enfors <christer.enfors@gmail.com>
;; Maintainer: Christer Enfors <christer.enfors@gmail.com>
;; Created: 2026
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1") (transient "0.3.7"))
;; Keywords: games
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
;;   - Action/Theme Oracle, inspired by the Mythic Game Master Emulator
;; - Generators:
;;   - NPC name and appearance
;;   - Dungeon rooms and random events
;;   - Wilderness random events
;;   - City random events
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
;; All cusomtization can be done by typing:
;;
;;   M-x customize-group RET solo-rpg RET
;;
;;   `solo-rpg-output-method' can be set to 'insert or 'message.
;;      - If set to 'message, output will be sent to message buffer only.
;;      - If set to 'insert, output will also be inserted in current buffer.
;;   `solo-rpg-auto-open-hud' can be set to t or nil, and determines whether
;;      or not the HUD which tracks Lonelog tags will be opened automatically.


;;; Code:

(require 'cl-lib)  ; For structs
(require 'subr-x)  ; For string-join
(require 'transient)

(defvar solo-rpg-mode)

;;; Customization variables:

(defgroup solo-rpg nil
  "Support for playing solo roleplaying games inside Emacs."
  :group 'games
  :prefix "solo-rpg-")

(defcustom solo-rpg-output-method 'insert
  "Default method for outputting solo-rpg results.
Can be `insert' to put text in current buffer, or `message' to only echo it."
  :type '(choice (const :tag "Insert in current buffer" insert)
                 (const :tag "Only show in message area" message))
  :group 'solo-rpg)

(defcustom solo-rpg-auto-open-hud t
  "If t, Solo-RPG-mode will auto-open the tag tracking buffer when started."
  :type 'boolean
  :group 'solo-rpg)

(defcustom solo-rpg-hud-update-delay 1.0
  "How many seconds of idle time before the HUD automatically updates."
  :type 'number
  :group 'solo-rpg)

(defcustom solo-rpg-hud-width 35
  "The width in characters of the Solo-RPG HUD window."
  :type 'number
  :group 'solo-rpg)


;;; TABLES:

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

;;; Yes/No oracle tables:

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

;;; Quantity oracle table:

(defconst solo-rpg-oracle-quantity-table
  '((1  . "Minimum")
    (3  . "Much less")
    (7  . "Less")
    (13 . "As expected")
    (17 . "More")
    (19 . "Much more")
    (20 . "Maximum"))
  "Data table for the Quantity oracle.
The `car` of each cell is the upper threshold for the `cdr` entry.")

;;; Generator tables:

(defconst solo-rpg-gen-plot-goal-table
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

(defconst solo-rpg-gen-plot-focus-table
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

(defconst solo-rpg-gen-plot-obstacle-table
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
;;; - Appearance:

(defconst solo-rpg-table-npc-height
  '((1  . "very short")
    (3  . "short")
    (7  . "somewhat short")
    (13 . "average")
    (17 . "somewhat tall")
    (19 . "tall")
    (20 . "very tall"))
  "Height data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-size
  '((1  . (:desc "very small"     :mod -3))
    (3  . (:desc "small"          :mod -2))
    (7  . (:desc "somewhat small" :mod -1))
    (13 . (:desc "average"        :mod 0))
    (17 . (:desc "somewhat large" :mod +1))
    (19 . (:desc "large"          :mod +2))
    (20 . (:desc "very large"     :mod +3)))
  "Size data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-eye-color
  '((2  . "light blue")
    (4  . "blue")
    (5  . "grey")
    (7  . "brown")
    (9  . "dark brown")
    (10 . "green"))
  "Eye color data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-skin-color
  '((5 . "western")
    (7 . "african")
    (8 . "asian"))
  "Skin color data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-hair-color
  '((2  . "blonde")
    (5  . "brown")
    (7  . "auburn")
    (8  . "red")
    (10 . "dark"))
  "Hair color data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-hair-length
  '((2 . "short")
    (3 . "shoulder length")
    (5 . "long")
    (6 . "very long"))
  "Hair length data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-long-hair-style
  '((2  . "loose")
    (4  . "pony tail")
    (5  . "bun")
    (7  . "braided")
    (9  . "half-up, half-down")
    (10 . "dreadlocks"))
  "Long hair style data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-facial-hair
  '((5  . "none")
    (7  . "beard")
    (8  . "mustache")
    (10 . "sideburns")
    (11 . "mutton chops")
    (12 . "goatee"))
  "Facial hair data table for the NPC Appearance generator.")

(defconst solo-rpg-table-npc-special-features
  '["facial scar"
    "facial birth mark"
    "piercings"
    "tattoos"
    "prominent nose"
    "distinctive eyebrows"
    "freckles"
    "thin lips"
    "full lips"
    "high cheekbones"
    "round face"
    "piercing gaze"
    "wide nose"
    "protruding ears"
    "cleft chin"
    "deep dimples"
    "pockmarked skin"
    "square jaw"
    "missing tooth"
    "broken/misshaped nose"]
  "Special features data table for the NPC Appearance generator.")

;;; Dungeon rooms:

(defvar solo-rpg-dungeon-room-exit-probs
  '((small  . ((forward . 40) (center . 60) (away . 40) (down . 15)))
    (medium . ((forward . 50) (center . 50) (away . 50) (down . 10)))
    (large  . ((forward . 60) (center . 40) (away . 60) (down .  5))))
  "The probabilities for each exit direction in each dungeon size.")

(defvar solo-rpg-dungeon-room-type-table
  '(( 8 . "corridor")
    (12 . "storage room")
    (14 . "ceremonial room")
    (15 . "study")
    (16 . "kitchen"))
  "Room type table for the dungeon room generator.")

(defvar solo-rpg-dungeon-room-contents-special
  '(("ceremonial" . ("altar"
                     "ceremonial circle"
                     "bottomless pit"))
    ("storage"    . ("crates"
                     "barrels"
                     "piles of junk"))
    ("study"      . ("book cases"
                     "desk"
                     "thick carpet"))))

;;; STRUCTS:

(cl-defstruct solo-rpg-dice-roll
  count    ;; Number of dice
  sides    ;; Die sizes
  mod      ;; Modifier (+/-)
  rolls    ;; List of each die roll result
  total)   ;; Sum of all rolls + mod

(cl-defstruct solo-rpg-deck
  "A formalized structure holding a deck of cards and its format logic."
  (type 'tarot)                     ; Typically `tarot' or `playcards'
  (name "Unnamed deck")             ; Name to display to humans
  (card-draw   #'ignore)            ; Function for drawing a card
  (card-format #'ignore)            ; Function for making card string
  (cards nil))                      ; The array of cards

;;; Tarot data and functions:

(defun solo-rpg-deck-tarot-card-draw (tarot-deck)
  "Return (and remove) one card with metadata plist from TAROT-DECK."
  (if (solo-rpg-deck-cards tarot-deck)
      (let ((card (nth (random (length (solo-rpg-deck-cards tarot-deck)))
                       (solo-rpg-deck-cards tarot-deck)))
            (metadata (list :reversed (= (random 2) 0))))
        (setf (solo-rpg-deck-cards tarot-deck)
              (delq card (solo-rpg-deck-cards tarot-deck)))
        (list :card card :metadata metadata))
    nil))

(defun solo-rpg-deck-tarot-card-text (card metadata)
  "Return a string representing CARD using METADATA."
  (format "%s%s\n%s"
          (plist-get card :name)
          (if (plist-get metadata :reversed)
              ", reversed"
            "")
          (if (plist-get metadata :reversed)
              (plist-get card :meanings-rev)
            (plist-get card :meanings-up))))

(defun solo-rpg-deck-tarot-copy ()
  "Return a deep copy of `solo-rpg-deck-tarot'."
  (let ((new-deck (copy-solo-rpg-deck solo-rpg-deck-tarot)))
    (setf (solo-rpg-deck-cards new-deck)
          (copy-sequence (solo-rpg-deck-cards solo-rpg-deck-tarot)))
    new-deck))

;; Tarot draw functions

(defun solo-rpg-deck-tarot-draw-single (&optional invert)
  "Draw a single Tarot card from DECK.
If INVERT is non-nil, then invert the output method."
  (interactive "P")
  (let ((card (solo-rpg-deck-tarot-card-draw solo-rpg-deck-tarot-active)))
    (solo-rpg--output (format "%s\n"
                              (solo-rpg-deck-tarot-card-text
                               (plist-get card :card)
                               (plist-get card :metadata))))))

(defconst solo-rpg-deck-tarot
  (make-solo-rpg-deck
   :type 'tarot
   :name "Rider-Waite-Smith tarot deck"
   :card-draw   #'solo-rpg-deck-tarot-card-draw
   :card-format #'solo-rpg-deck-tarot-card-text
   :cards
   '(
     ;; ----------------------------------------
     ;; Major Arcana
     ;; ----------------------------------------
     (:id 0
          :name "The Fool"
          :type major
          :meanings-up "Folly, mania, extravagance, intoxication, delirium, \
frenzy, bewrayment."
          :meanings-rev "Negligence, absence, distribution, carelessness, \
apathy, nullity, vanity."
          :image-file "tarot_ar00.jpg")
     (:id 1
          :name "The Magician"
          :type major
          :meanings-up "Skill, diplomacy, address, subtlety; sickness, \
pain, loss, disaster, snares of enemies; self-confidence, will; the \
Querent, if male."
          :meanings-rev "Physician, Magus, mental disease, disgrace, \
disquiet."
          :image-file "tarot_ar01.jpg")
     (:id 2
          :name "The High Priestess"
          :type major
          :meanings-up "Secrets, mystery, the future as yet unrevealed; \
the woman who interests the Querent, if male; the Querent herself, if \
female; silence, tenacity; mystery, wisdom, science."
          :meanings-rev "Passion, moral or physical ardour, conceit, \
surface knowledge."
          :image-file "tarot_ar02.jpg")
     (:id 3
          :name "The Empress"
          :type major
          :meanings-up "Fruitfulness, action, initiative, length of days; \
the unknown, clandestine; also difficulty, doubt, ignorance."
          :meanings-rev "Light, truth, the unravelling of involved matters, \
public rejoicings; according to another reading, vacillation."
          :image-file "tarot_ar03.jpg")
     (:id 4
          :name "The Emperor"
          :type major
          :meanings-up "Stability, power, protection, realization; a great \
person; aid, reason, conviction; also authority and will."
          :meanings-rev "Benevolence, compassion, credit; also confusion \
to enemies, obstruction, immaturity."
          :image-file "tarot_ar04.jpg")
     (:id 5
          :name "The Hierophant"
          :type major
          :meanings-up "Marriage, alliance, captivity, servitude; by \
another account, mercy and goodness; inspiration; the man to whom the \
Querent has recourse."
          :meanings-rev "Society, good understanding, concord, \
overkindness, weakness."
          :image-file "tarot_ar05.jpg")
     (:id 6
          :name "The Lovers"
          :type major
          :meanings-up "Attraction, love, beauty, trials overcome."
          :meanings-rev "Failure, foolish designs. Another account speaks \
of marriage frustrated and contrarieties of all kinds."
          :image-file "tarot_ar06.jpg")
     (:id 7
          :name "The Chariot"
          :type major
          :meanings-up "Succour, providence also war, triumph, \
presumption, vengeance, trouble."
          :meanings-rev "Riot, quarrel, dispute, litigation, defeat."
          :image-file "tarot_ar07.jpg")
     (:id 8
          :name "Fortitude"
          :type major
          :meanings-up "Power, energy, action, courage, magnanimity; \
also complete success and honours."
          :meanings-rev "Despotism, abuse if power, weakness, discord, \
sometimes even disgrace."
          :image-file "tarot_ar08.jpg")
     (:id 9
          :name "The Hermit"
          :type major
          :meanings-up "Prudence, circumspection; also and especially \
treason, dissimulation, roguery, corruption."
          :meanings-rev "Concealment, disguise, policy, fear, unreasoned \
caution."
          :image-file "tarot_ar09.jpg")
     (:id 10
          :name "Wheel Of Fortune"
          :type major
          :meanings-up "Destiny, fortune, success, elevation, luck, \
felicity."
          :meanings-rev "Increase, abundance, superfluity."
          :image-file "tarot_ar10.jpg")
     (:id 11
          :name "Justice"
          :type major
          :meanings-up "Equity, rightness, probity, executive; triumph \
of the deserving side in law."
          :meanings-rev "Law in all its departments, legal complications, \
bigotry, bias, excessive severity."
          :image-file "tarot_ar11.jpg")
     (:id 12
          :name "The Hanged Man"
          :type major
          :meanings-up "Wisdom, circumspection, discernment, trials, \
sacrifice, intuition, divination, prophecy."
          :meanings-rev "Selfishness, the crowd, body politic."
          :image-file "tarot_ar12.jpg")
     (:id 13
          :name "Death"
          :type major
          :meanings-up "End, mortality, destruction, corruption also, \
for a man, the loss of a benefactor for a woman, many contrarieties; \
for a maid, failure of marriage projects."
          :meanings-rev "Inertia, sleep, lethargy, petrifaction, \
somnambulism; hope destroyed."
          :image-file "tarot_ar13.jpg")
     (:id 14
          :name "Temperance"
          :type major
          :meanings-up "Economy, moderation, frugality, management, \
accommodation."
          :meanings-rev "Things connected with churches, religions, sects, \
the priesthood, sometimes even the priest who will marry the Querent; \
also disunion, unfortunate combinations, competing interests."
          :image-file "tarot_ar14.jpg")
     (:id 15
          :name "The Devil"
          :type major
          :meanings-up "Ravage, violence, vehemence, extraordinary \
efforts, force, fatality; that which is predestined but is not for \
this reason evil."
          :meanings-rev "Evil fatality, weakness, pettiness, blindness."
          :image-file "tarot_ar15.jpg")
     (:id 16
          :name "The Tower"
          :type major
          :meanings-up "Misery, distress, indigence, adversity, \
calamity, disgrace, deception, ruin. It is a card in particular of \
unforeseen catastrophe."
          :meanings-rev "According to one account, the same in a lesser \
degree also oppression, imprisonment, tyranny."
          :image-file "tarot_ar16.jpg")
     (:id 17
          :name "The Star"
          :type major
          :meanings-up "Loss, theft, privation, abandonment; another \
reading says-hope and bright prospects,"
          :meanings-rev "Arrogance, haughtiness, impotence."
          :image-file "tarot_ar17.jpg")
     (:id 18
          :name "The Moon"
          :type major
          :meanings-up "Hidden enemies, danger, calumny, darkness, \
terror, deception, occult forces, error."
          :meanings-rev "Instability, inconstancy, silence, lesser \
degrees of deception and error."
          :image-file "tarot_ar18.jpg")
     (:id 19
          :name "The Sun"
          :type major
          :meanings-up "Material happiness, fortunate marriage, \
contentment."
          :meanings-rev "The same in a lesser sense."
          :image-file "tarot_ar19.jpg")
     (:id 20
          :name "The Last Judgment"
          :type major
          :meanings-up "Change of position, renewal, outcome. Another \
account specifies total loss though lawsuit."
          :meanings-rev "Weakness, pusillanimity, simplicity; also \
deliberation, decision, sentence."
          :image-file "tarot_ar20.jpg")
     (:id 21
          :name "The World"
          :type major
          :meanings-up "Assured success, recompense, voyage, route, \
emigration, flight, change of place."
          :meanings-rev "Inertia, fixity, stagnation, permanence."
          :image-file "tarot_ar21.jpg")

     ;; ----------------------------------------
     ;; Suit of Wands
     ;; ----------------------------------------
     (:id 1
          :name "Ace of Wands"
          :type minor
          :suit wands
          :meanings-up "Creation, invention, enterprise, the powers \
which result in these; principle, beginning, source; birth, family, \
origin, and in a sense the virility which is behind them; the starting \
point of enterprises; according to another account, money, fortune, \
inheritance."
          :meanings-rev "Fall, decadence, ruin, perdition, to perish \
also a certain clouded joy."
          :image-file "tarot_waac.jpg")
     (:id 2
          :name "Two of Wands"
          :type minor
          :suit wands
          :meanings-up "Between the alternative readings there is no \
marriage possible; on the one hand, riches, fortune, magnificence; on \
the other, physical suffering, disease, chagrin, sadness, \
mortification. The design gives one suggestion; here is a lord \
overlooking his dominion and alternately contemplating a globe; it \
looks like the malady, the mortification, the sadness of Alexander \
amidst the grandeur of this world's wealth."
          :meanings-rev "Surprise, wonder, enchantment, emotion, trouble, \
fear."
          :image-file "tarot_wa02.jpg")
     (:id 3
          :name "Three of Wands"
          :type minor
          :suit wands
          :meanings-up "He symbolizes established strength, enterprise, \
effort, trade, commerce, discovery; those are his ships, bearing his \
merchandise, which are sailing over the sea. The card also signifies \
able co-operation in business, as if the successful merchant prince \
were looking from his side towards yours with a view to help you."
          :meanings-rev "The end of troubles, suspension or cessation \
of adversity, toil and disappointment."
          :image-file "tarot_wa03.jpg")
     (:id 4
          :name "Four of Wands"
          :type minor
          :suit wands
          :meanings-up "They are for once almost on the surface--country \
life, haven of refuge, a species of domestic harvest-home, repose, \
concord, harmony, prosperity, peace, and the perfected work of these."
          :meanings-rev "The meaning remains unaltered; it is prosperity, \
increase, felicity, beauty, embellishment."
          :image-file "tarot_wa04.jpg")
     (:id 5
          :name "Five of Wands"
          :type minor
          :suit wands
          :meanings-up "Imitation, as, for example, sham fight, but \
also the strenuous competition and struggle of the search after riches \
and fortune. In this sense it connects with the battle of life. Hence \
some attributions say that it is a card of gold, gain, opulence."
          :meanings-rev "Litigation, disputes, trickery, contradiction."
          :image-file "tarot_wa05.jpg")
     (:id 6
          :name "Six of Wands"
          :type minor
          :suit wands
          :meanings-up "The card has been so designed that it can cover \
several significations; on the surface, it is a victor triumphing, but \
it is also great news, such as might be carried in state by the King's \
courier; it is expectation crowned with its own desire, the crown of \
hope, and so forth."
          :meanings-rev "Apprehension, fear, as of a victorious enemy \
at the gate; treachery, disloyalty, as of gates being opened to the \
enemy; also indefinite delay."
          :image-file "tarot_wa06.jpg")
     (:id 7
          :name "Seven of Wands"
          :type minor
          :suit wands
          :meanings-up "It is a card of valour, for, on the surface, \
six are attacking one, who has, however, the vantage position. On the \
intellectual plane, it signifies discussion, wordy strife; in \
business--negotiations, war of trade, barter, competition. It is \
further a card of success, for the combatant is on the top and his \
enemies may be unable to reach him."
          :meanings-rev "Perplexity, embarrassments, anxiety. It is \
also a caution against indecision."
          :image-file "tarot_wa07.jpg")
     (:id 8
          :name "Eight of Wands"
          :type minor
          :suit wands
          :meanings-up "Activity in undertakings, the path of such \
activity, swiftness, as that of an express messenger; great haste, \
great hope, speed towards an end which promises assured felicity; \
generally, that which is on the move; also the arrows of love."
          :meanings-rev "Arrows of jealousy, internal dispute, stingings \
of conscience, quarrels; and domestic disputes for persons who are \
married."
          :image-file "tarot_wa08.jpg")
     (:id 9
          :name "Nine of Wands"
          :type minor
          :suit wands
          :meanings-up "The card signifies strength in opposition. If \
attacked, the person will meet an onslaught boldly; and his build \
shews, that he may prove a formidable antagonist. With this main \
significance there are all its possible adjuncts--delay, suspension, \
adjournment."
          :meanings-rev "Obstacles, adversity, calamity."
          :image-file "tarot_wa09.jpg")
     (:id 10
          :name "Ten of Wands"
          :type minor
          :suit wands
          :meanings-up "A card of many significances, and some of the \
readings cannot be harmonized. I set aside that which connects it with \
honour and good faith. The chief meaning is oppression simply, but it \
is also fortune, gain, any kind of success, and then it is the \
oppression of these things. It is also a card of false-seeming, \
disguise, perfidy. The place which the figure is approaching may suffer \
from the rods that he carries. Success is stultified if the Nine of \
Swords follows, and if it is a question of a lawsuit, there will be \
certain loss."
          :meanings-rev "Contrarieties, difficulties, intrigues, and \
their analogies."
          :image-file "tarot_wa10.jpg")
     (:id 11
          :name "Page of Wands"
          :type minor
          :suit wands
          :meanings-up "Dark young man, faithful, a lover, an envoy, a \
postman. Beside a man, he will bear favourable testimony concerning him. \
A dangerous rival, if followed by the Page of Cups. Has the chief \
qualities of his suit. He may signify family intelligence."
          :meanings-rev "Anecdotes, announcements, evil news. Also \
indecision and the instability which accompanies it."
          :image-file "tarot_wapa.jpg")
     (:id 12
          :name "Knight of Wands"
          :type minor
          :suit wands
          :meanings-up "Departure, absence, flight, emigration. A dark \
young man, friendly. Change of residence."
          :meanings-rev "Rupture, division, interruption, discord."
          :image-file "tarot_wakn.jpg")
     (:id 13
          :name "Queen of Wands"
          :type minor
          :suit wands
          :meanings-up "A dark woman, countrywoman, friendly, chaste, \
loving, honourable. If the card beside her signifies a man, she is \
well disposed towards him; if a woman, she is interested in the \
Querent. Also, love of money, or a certain success in business."
          :meanings-rev "Good, economical, obliging, serviceable. \
Signifies also--but in certain positions and in the neighbourhood of \
other cards tending in such directions--opposition, jealousy, even \
deceit and infidelity."
          :image-file "tarot_waqu.jpg")
     (:id 14
          :name "King of Wands"
          :type minor
          :suit wands
          :meanings-up "Dark man, friendly, countryman, generally \
married, honest and conscientious. The card always signifies honesty, \
and may mean news concerning an unexpected heritage to fall in before \
very long."
          :meanings-rev "Good, but severe; austere, yet tolerant."
          :image-file "tarot_waki.jpg")

     ;; ----------------------------------------
     ;; Suit of Cups
     ;; ----------------------------------------
     (:id 1
          :name "Ace of Cups"
          :type minor
          :suit cups
          :meanings-up "House of the true heart, joy, content, abode, \
nourishment, abundance, fertility; Holy Table, felicity hereof."
          :meanings-rev "House of the false heart, mutation, \
instability, revolution."
          :image-file "tarot_cuac.jpg")
     (:id 2
          :name "Two of Cups"
          :type minor
          :suit cups
          :meanings-up "Love, passion, friendship, affinity, union, \
concord, sympathy, the interrelation of the sexes, and--as a suggestion \
apart from all offices of divination--that desire which is not in \
Nature, but by which Nature is sanctified."
          :meanings-rev "Lust, cupidity, jealousy, wish, desire, but \
the card may also give, says W., \"that desire which is not in nature, \
but by which nature is sanctified.\""
          :image-file "tarot_cu02.jpg")
     (:id 3
          :name "Three of Cups"
          :type minor
          :suit cups
          :meanings-up "The conclusion of any matter in plenty, \
perfection and merriment; happy issue, victory, fulfilment, solace, \
healing,"
          :meanings-rev "Expedition, dispatch, achievement, end. It \
signifies also the side of excess in physical enjoyment, and the \
pleasures of the senses."
          :image-file "tarot_cu03.jpg")
     (:id 4
          :name "Four of Cups"
          :type minor
          :suit cups
          :meanings-up "Weariness, disgust, aversion, imaginary \
vexations, as if the wine of this world had caused satiety only; \
another wine, as if a fairy gift, is now offered the wastrel, but he \
sees no consolation therein. This is also a card of blended pleasure."
          :meanings-rev "Novelty, presage, new instruction, new \
relations."
          :image-file "tarot_cu04.jpg")
     (:id 5
          :name "Five of Cups"
          :type minor
          :suit cups
          :meanings-up "A dark, cloaked figure, looking sideways at \
three prone cups two others stand upright behind him; a bridge is in \
the background, leading to a small keep or holding. Divanatory \
Meanings: It is a card of loss, but something remains over; three have \
been taken, but two are left; it is a card of inheritance, patrimony, \
transmission, but not corresponding to expectations; with some \
interpreters it is a card of marriage, but not without bitterness or \
frustration."
          :meanings-rev "News, alliances, affinity, consanguinity, \
ancestry, return, false projects."
          :image-file "tarot_cu05.jpg")
     (:id 6
          :name "Six of Cups"
          :type minor
          :suit cups
          :meanings-up "A card of the past and of memories, looking \
back, as--for example--on childhood; happiness, enjoyment, but coming \
rather from the past; things that have vanished. Another reading \
reverses this, giving new relations, new knowledge, new environment, \
and then the children are disporting in an unfamiliar precinct."
          :meanings-rev "The future, renewal, that which will come to \
pass presently."
          :image-file "tarot_cu06.jpg")
     (:id 7
          :name "Seven of Cups"
          :type minor
          :suit cups
          :meanings-up "Fairy favours, images of reflection, sentiment, \
imagination, things seen in the glass of contemplation; some attainment \
in these degrees, but nothing permanent or substantial is suggested."
          :meanings-rev "Desire, will, determination, project."
          :image-file "tarot_cu07.jpg")
     (:id 8
          :name "Eight of Cups"
          :type minor
          :suit cups
          :meanings-up "The card speaks for itself on the surface, but \
other readings are entirely antithetical--giving joy, mildness, \
timidity, honour, modesty. In practice, it is usually found that the \
card shews the decline of a matter, or that a matter which has been \
thought to be important is really of slight consequence--either for \
good or evil."
          :meanings-rev "Great joy, happiness, feasting."
          :image-file "tarot_cu08.jpg")
     (:id 9
          :name "Nine of Cups"
          :type minor
          :suit cups
          :meanings-up "Concord, contentment, physical bien-être; also \
victory, success, advantage; satisfaction for the Querent or person \
for whom the consultation is made."
          :meanings-rev "Truth, loyalty, liberty; but the readings vary \
and include mistakes, imperfections, etc."
          :image-file "tarot_cu09.jpg")
     (:id 10
          :name "Ten of Cups"
          :type minor
          :suit cups
          :meanings-up "Contentment, repose of the entire heart; the \
perfection of that state; also perfection of human love and friendship; \
if with several picture-cards, a person who is taking charge of the \
Querent's interests; also the town, village or country inhabited by \
the Querent."
          :meanings-rev "Repose of the false heart, indignation, \
violence."
          :image-file "tarot_cu10.jpg")
     (:id 11
          :name "Page of Cups"
          :type minor
          :suit cups
          :meanings-up "Fair young man, one impelled to render service \
and with whom the Querent will be connected; a studious youth; news, \
message; application, reflection, meditation; also these things \
directed to business."
          :meanings-rev "Taste, inclination, attachment, seduction, \
deception, artifice."
          :image-file "tarot_cupa.jpg")
     (:id 12
          :name "Knight of Cups"
          :type minor
          :suit cups
          :meanings-up "Arrival, approach--sometimes that of a \
messenger; advances, proposition, demeanour, invitation, incitement."
          :meanings-rev "Trickery, artifice, subtlety, swindling, \
duplicity, fraud."
          :image-file "tarot_cukn.jpg")
     (:id 13
          :name "Queen of Cups"
          :type minor
          :suit cups
          :meanings-up "Good, fair woman; honest, devoted woman, who \
will do service to the Querent; loving intelligence, and hence the \
gift of vision; success, happiness, pleasure; also wisdom, virtue; a \
perfect spouse and a good mother."
          :meanings-rev "The accounts vary; good woman; otherwise, \
distinguished woman but one not to be trusted; perverse woman; vice, \
dishonour, depravity."
          :image-file "tarot_cuqu.jpg")
     (:id 14
          :name "King of Cups"
          :type minor
          :suit cups
          :meanings-up "Fair man, man of business, law, or divinity; \
responsible, disposed to oblige the Querent; also equity, art and \
science, including those who profess science, law and art; creative \
intelligence."
          :meanings-rev "Dishonest, double-dealing man; roguery, \
exaction, injustice, vice, scandal, pillage, considerable loss."
          :image-file "tarot_cuki.jpg")

     ;; ----------------------------------------
     ;; Suit of Pentacles
     ;; ----------------------------------------
     (:id 1
          :name "Ace of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Perfect contentment, felicity, ecstasy; also \
speedy intelligence; gold."
          :meanings-rev "The evil side of wealth, bad intelligence; \
also great riches. In any case it shews prosperity, comfortable \
material conditions, but whether these are of advantage to the \
possessor will depend on whether the card is reversed or not."
          :image-file "tarot_peac.jpg")
     (:id 2
          :name "Two of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "On the one hand it is represented as a card of \
gaiety, recreation and its connexions, which is the subject of the \
design; but it is read also as news and messages in writing, as \
obstacles, agitation, trouble, embroilment."
          :meanings-rev "Enforced gaiety, simulated enjoyment, literal \
sense, handwriting, composition, letters of exchange."
          :image-file "tarot_pe02.jpg")
     (:id 3
          :name "Three of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Métier, trade, skilled labour; usually, \
however, regarded as a card of nobility, aristocracy, renown, glory."
          :meanings-rev "Mediocrity, in work and otherwise, puerility, \
pettiness, weakness."
          :image-file "tarot_pe03.jpg")
     (:id 4
          :name "Four of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "The surety of possessions, cleaving to that \
which one has, gift, legacy, inheritance."
          :meanings-rev "Suspense, delay, opposition."
          :image-file "tarot_pe04.jpg")
     (:id 5
          :name "Five of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "The card foretells material trouble above all, \
whether in the form illustrated--that is, destitution--or otherwise. \
For some cartomancists, it is a card of love and lovers-wife, husband, \
friend, mistress; also concordance, affinities. These alternatives \
cannot be harmonized."
          :meanings-rev "Disorder, chaos, ruin, discord, profligacy."
          :image-file "tarot_pe05.jpg")
     (:id 6
          :name "Six of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Presents, gifts, gratification another account \
says attention, vigilance now is the accepted time, present \
prosperity, etc."
          :meanings-rev "Desire, cupidity, envy, jealousy, illusion."
          :image-file "tarot_pe06.jpg")
     (:id 7
          :name "Seven of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "These are exceedingly contradictory; in the \
main, it is a card of money, business, barter; but one reading gives \
altercation, quarrels--and another innocence, ingenuity, purgation."
          :meanings-rev "Cause for anxiety regarding money which it may \
be proposed to lend."
          :image-file "tarot_pe07.jpg")
     (:id 8
          :name "Eight of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Work, employment, commission, craftsmanship, \
skill in craft and business, perhaps in the preparatory stage."
          :meanings-rev "Voided ambition, vanity, cupidity, exaction, \
usury. It may also signify the possession of skill, in the sense of \
the ingenious mind turned to cunning and intrigue."
          :image-file "tarot_pe08.jpg")
     (:id 9
          :name "Nine of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Prudence, safety, success, accomplishment, \
certitude, discernment."
          :meanings-rev "Roguery, deception, voided project, bad faith."
          :image-file "tarot_pe09.jpg")
     (:id 10
          :name "Ten of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Gain, riches; family matters, archives, \
extraction, the abode of a family."
          :meanings-rev "Chance, fatality, loss, robbery, games of \
hazard; sometimes gift, dowry, pension."
          :image-file "tarot_pe10.jpg")
     (:id 11
          :name "Page of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Application, study, scholarship, reflection \
another reading says news, messages and the bringer thereof; also \
rule, management."
          :meanings-rev "Prodigality, dissipation, liberality, luxury; \
unfavourable news."
          :image-file "tarot_pepa.jpg")
     (:id 12
          :name "Knight of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Utility, serviceableness, interest, \
responsibility, rectitude-all on the normal and external plane."
          :meanings-rev "inertia, idleness, repose of that kind, \
stagnation; also placidity, discouragement, carelessness."
          :image-file "tarot_pekn.jpg")
     (:id 13
          :name "Queen of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Opulence, generosity, magnificence, security, \
liberty."
          :meanings-rev "Evil, suspicion, suspense, fear, mistrust."
          :image-file "tarot_pequ.jpg")
     (:id 14
          :name "King of Pentacles"
          :type minor
          :suit pentacles
          :meanings-up "Valour, realizing intelligence, business and \
normal intellectual aptitude, sometimes mathematical gifts and \
attainments of this kind; success in these paths."
          :meanings-rev "Vice, weakness, ugliness, perversity, \
corruption, peril."
          :image-file "tarot_peki.jpg")

     ;; ----------------------------------------
     ;; Suit of Swords
     ;; ----------------------------------------
     (:id 1
          :name "Ace of Swords"
          :type minor
          :suit swords
          :meanings-up "Triumph, the excessive degree in everything, \
conquest, triumph of force. It is a card of great force, in love as \
well as in hatred. The crown may carry a much higher significance than \
comes usually within the sphere of fortune-telling."
          :meanings-rev "The same, but the results are disastrous; \
another account says--conception, childbirth, augmentation, \
multiplicity."
          :image-file "tarot_swac.jpg")
     (:id 2
          :name "Two of Swords"
          :type minor
          :suit swords
          :meanings-up "Conformity and the equipoise which it suggests, \
courage, friendship, concord in a state of arms; another reading gives \
tenderness, affection, intimacy. The suggestion of harmony and other \
favourable readings must be considered in a qualified manner, as \
Swords generally are not symbolical of beneficent forces in human \
affairs."
          :meanings-rev "Imposture, falsehood, duplicity, disloyalty."
          :image-file "tarot_sw02.jpg")
     (:id 3
          :name "Three of Swords"
          :type minor
          :suit swords
          :meanings-up "Removal, absence, delay, division, rupture, \
dispersion, and all that the design signifies naturally, being too \
simple and obvious to call for specific enumeration."
          :meanings-rev "Mental alienation, error, loss, distraction, \
disorder, confusion."
          :image-file "tarot_sw03.jpg")
     (:id 4
          :name "Four of Swords"
          :type minor
          :suit swords
          :meanings-up "Vigilance, retreat, solitude, hermit's repose, \
exile, tomb and coffin. It is these last that have suggested the \
design."
          :meanings-rev "Wise administration, circumspection, economy, \
avarice, precaution, testament."
          :image-file "tarot_sw04.jpg")
     (:id 5
          :name "Five of Swords"
          :type minor
          :suit swords
          :meanings-up "Degradation, destruction, revocation, infamy, \
dishonour, loss, with the variants and analogues of these."
          :meanings-rev "The same; burial and obsequies."
          :image-file "tarot_sw05.jpg")
     (:id 6
          :name "Six of Swords"
          :type minor
          :suit swords
          :meanings-up "Journey by water, route, way, envoy, \
commissionary, expedient."
          :meanings-rev "Declaration, confession, publicity; one \
account says that it is a proposal of love."
          :image-file "tarot_sw06.jpg")
     (:id 7
          :name "Seven of Swords"
          :type minor
          :suit swords
          :meanings-up "Design, attempt, wish, hope, confidence; also \
quarrelling, a plan that may fail, annoyance. The design is uncertain \
in its import, because the significations are widely at variance with \
each other."
          :meanings-rev "Good advice, counsel, instruction, slander, \
babbling."
          :image-file "tarot_sw07.jpg")
     (:id 8
          :name "Eight of Swords"
          :type minor
          :suit swords
          :meanings-up "Bad news, violent chagrin, crisis, censure, \
power in trammels, conflict, calumny; also sickness."
          :meanings-rev "Disquiet, difficulty, opposition, accident, \
treachery; what is unforeseen; fatality."
          :image-file "tarot_sw08.jpg")
     (:id 9
          :name "Nine of Swords"
          :type minor
          :suit swords
          :meanings-up "Death, failure, miscarriage, delay, deception, \
disappointment, despair."
          :meanings-rev "Imprisonment, suspicion, doubt, reasonable \
fear, shame."
          :image-file "tarot_sw09.jpg")
     (:id 10
          :name "Ten of Swords"
          :type minor
          :suit swords
          :meanings-up "Whatsoever is intimated by the design; also \
pain, affliction, tears, sadness, desolation. It is not especially a \
card of violent death."
          :meanings-rev "Advantage, profit, success, favour, but none \
of these are permanent; also power and authority."
          :image-file "tarot_sw10.jpg")
     (:id 11
          :name "Page of Swords"
          :type minor
          :suit swords
          :meanings-up "Authority, overseeing, secret service, \
vigilance, spying, examination, and the qualities thereto belonging."
          :meanings-rev "More evil side of these qualities; what is \
unforeseen, unprepared state; sickness is also intimated."
          :image-file "tarot_swpa.jpg")
     (:id 12
          :name "Knight of Swords"
          :type minor
          :suit swords
          :meanings-up "Skill, bravery, capacity, defence, address, \
enmity, wrath, war, destruction, opposition, resistance, ruin. There \
is therefore a sense in which the card signifies death, but it carries \
this meaning only in its proximity to other cards of fatality."
          :meanings-rev "Imprudence, incapacity, extravagance."
          :image-file "tarot_swkn.jpg")
     (:id 13
          :name "Queen of Swords"
          :type minor
          :suit swords
          :meanings-up "Widowhood, female sadness and embarrassment, \
absence, sterility, mourning, privation, separation."
          :meanings-rev "Malice, bigotry, artifice, prudery, bale, \
deceit."
          :image-file "tarot_swqu.jpg")
     (:id 14
          :name "King of Swords"
          :type minor
          :suit swords
          :meanings-up "Whatsoever arises out of the idea of judgment \
and all its connexions-power, command, authority, militant \
intelligence, law, offices of the crown, and so forth."
          :meanings-rev "Cruelty, perversity, barbarity, perfidy, evil \
intention."
          :image-file "tarot_swki.jpg"))))

;;; OTHER VARIABLES:

(defvar solo-rpg--last-dice-string "2d6"
  "The last dice string rolled by the user. Used in the default prompt.")

(defvar solo-rpg--return-buffer nil
  "Remembers which buffer to insert the final text into.")

(defvar solo-rpg--staging-buffer-name "*solo-rpg-staging*"
  "The name of our temporary staging area buffer.")

(defvar solo-rpg--generate-fun nil
  "The callback function which returns generated text for staging.")

(defvar solo-rpg-npc-nsfw 'off
  "Whether or not to generate NSFW options for NPCs.")

(defvar solo-rpg-npc-facial-hair 'off
  "Whether or not to generate facial hair for NPCs.")

(defvar solo-rpg-dungeon-size 'medium
  "Size to take into consideration when generating dungeon rooms.")

(defvar-local solo-rpg--hud-timer nil
  "Buffer-local variable to store the active HUD timer for this session.")

(defvar-local solo-rpg--hud-buffer-name nil
  "Buffer-local variable storing the unique name of this session's HUD.")

(defvar-local solo-rpg-deck-tarot-active nil
  "The copy of the Tarot deck currently in use.")


;;; FUNCTIONS:
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


;;; Staging functions

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
  (call-interactively #'solo-rpg-menu-staging))

(defun solo-rpg--staging-update (text)
  "Wipe the staging buffer, insert TEXT, and show it at the bottom."
  (with-current-buffer (get-buffer-create solo-rpg--staging-buffer-name)
    (erase-buffer)
    (insert text)
    (goto-char (point-min)) ;; Always scroll to the top of the new text

    ;; Only ask Emacs to arrange the window if it isn't already visible!
    (unless (get-buffer-window (current-buffer))
      (display-buffer (current-buffer)
                      '(display-buffer-at-bottom . ((window-height . 14)))))))

;; Standard function for regeneration

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


;;; Table functions

(defun solo-rpg-table-get-random (table)
  "Return random element from TABLE."
  (aref table (random (length table))))

(defun solo-rpg--table-weighted-get-random (weighted-table)
  "Return random element from WEIGHTED-TABLE."
  (let* ((max-value (caar (last weighted-table)))
         (roll (+ 1 (random max-value))))
    (cl-loop for cell in weighted-table
             for threshold = (car cell)
             for label     = (cdr cell)
             if (<= roll threshold)
             return label)))

(defun solo-rpg--table-weighted-get-random-single (prefix var)
  "Construct full var name from PREFIX+VAR, get value from table."
  (let ((table-symbol (intern (format "solo-rpg-%s-%s-table" prefix var))))
    (solo-rpg--table-weighted-get-random (symbol-value table-symbol))))

(defun solo-rpg--table-weighted-get-random-list (prefix var-list)
  "Construct full var name from PREFIX+VAR-LIST elems, get value from table."
  (mapcar (lambda (var)
            (cons (intern var)
                  (solo-rpg--table-weighted-get-random-single prefix var)))
          var-list))


;;; Dashboard functions

(defun solo-rpg-output-method-toggle ()
  "Toggle `solo-rpg-output-method' between `insert' and `message'."
  (interactive)
  (if (eq solo-rpg-output-method 'insert)
      (setq solo-rpg-output-method 'message)
    (setq solo-rpg-output-method 'insert))
  (message "Solo-RPG output method is now: %s" solo-rpg-output-method))

(defun solo-rpg--toggle-output-desc ()
  "Return a formatted string showing the current output method."
  (format "Output: %s" solo-rpg-output-method))

(defun solo-rpg-toggle-npc-nsfw ()
  "Toggle `solo-rpg-npc-nsfw' between `on' and `off'."
  (interactive)
  (if (eq solo-rpg-npc-nsfw 'on)
      (setq solo-rpg-npc-nsfw 'off)
    (setq solo-rpg-npc-nsfw 'on)))

(defun solo-rpg--toggle-npc-nsfw-desc ()
  "Return a string showing the current state of `solo-rpg-npc-nsfw'."
  (format "NSFW=%s" solo-rpg-npc-nsfw))
  
(defun solo-rpg-toggle-npc-facial-hair ()
  "Toggle `solo-rpg-npc-facial-hair' between `on' and `off'."
  (interactive)
  (if (eq solo-rpg-npc-facial-hair 'on)
      (setq solo-rpg-npc-facial-hair 'off)
    (setq solo-rpg-npc-facial-hair 'on)))

(defun solo-rpg--toggle-npc-facial-hair-desc ()
  "Return a string showing the current state of `solo-rpg-npc-facial-hair'."
  (format "Facial hair=%s" solo-rpg-npc-facial-hair))

(defun solo-rpg-toggle-dungeon-size ()
  "Toggle `solo-rpg-dungeon-size' between `small', `medium', and `large'."
  (interactive)
  (cond ((eq solo-rpg-dungeon-size 'large)
         (setq solo-rpg-dungeon-size 'small))
        ((eq solo-rpg-dungeon-size 'medium)
         (setq solo-rpg-dungeon-size 'large))
        ((eq solo-rpg-dungeon-size 'small)
         (setq solo-rpg-dungeon-size 'medium))))

(defun solo-rpg--toggle-dungeon-size-desc ()
  "Return a string showing the current state of `solo-rpg-dungeon-size'."
  (format "Dungeon size=%s" solo-rpg-dungeon-size))

;;; Dice roll functions

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
         (rolls-string (if (< 1 (length (solo-rpg-dice-roll-rolls roll-struct)))
                           (concat "="
                                   (mapconcat (lambda (x) (format "%d" x))
                                              (solo-rpg-dice-roll-rolls roll-struct)
                                              ",")
                                   mod-string)
                         "")))
    (format "%dd%d%s%s=%d"
            (solo-rpg-dice-roll-count roll-struct)
            (solo-rpg-dice-roll-sides roll-struct)
            mod-string
            rolls-string
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


;;; ORACLES:
;;; Action / Theme oracle:

(defun solo-rpg-oracle-action-theme (&optional invert)
  "Output (action) / (theme) where action and theme are random.
If INVERT is non-nil, then output mode is inverted."
  (interactive "P")
  (solo-rpg--output (format "%s / %s"
                            (solo-rpg-table-get-random solo-rpg-oracle-actions)
                            (solo-rpg-table-get-random solo-rpg-oracle-themes))
                    invert))

;;; Yes / No oracle

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
    (format "%s: 1d20=%d -> %s"
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


;;; Quantity oracle

(defun solo-rpg-oracle-quantity (&optional invert)
  "Query the Quantity oracle, displaying the result.
If INVERT is non-nil, then output is inverted."
  (interactive "P")
  (solo-rpg--output (solo-rpg--table-weighted-get-random
                     solo-rpg-oracle-quantity-table)
                    invert))


;;; GENERATORS:
;;; Plot generator:

(defun solo-rpg--gen-plot-text ()
  "Generate and return a plot text."
  (format "%s %s, but %s"
           (solo-rpg-table-get-random solo-rpg-gen-plot-goal-table)
           (solo-rpg-table-get-random solo-rpg-gen-plot-focus-table)
           (solo-rpg-table-get-random solo-rpg-gen-plot-obstacle-table)))

(defun solo-rpg-gen-plot ()
  "Generate a plot and open it in the staging area."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-plot-text))

;;; Narrative event generator:

(defconst solo-rpg-gen-nar-event-incident-table
  ["An assumption is proven wrong"
   "A previously solved problem returns"
   "Someone makes an unexpected appearance"
   "A faction acts"
   "Someone is put on the spot"
   "A resource becomes unavailable"
   "An environmental shift or hazard alters the scene"
   "A hidden motive is revealed"
   "A sudden obstacle blocks the path"
   "A valuable opportunity presents itself"
   "A piece of local lore becomes relevant"
   "An unknown entity or force enters the scene"
   "A loud or obvious distraction occurs nearby"
   "Someone or something comes under sudden attack"
   "A cruicial piece of equipment fails"]
  "Incidents for the narrative event generator.")

(defconst solo-rpg-gen-nar-event-twist-table
  ["but it comes with a hidden cost or catch"
   "and a timer starts; something must be done immediately"
   "and it creates a new, unrelated problem"
   "and it demands an immediate reaction or difficult choice"
   "but it may not be what it first appears to be"
   "which endangers an innocent bystander or ally"
   "which reveals a critical vulnerability"
   "which immediately attracts attention"
   "and someone is isolated as a result"
   "which provides a fleeting, temporary advantage"
   "which shifts the balance of power"
   "and the layout of the situation is shifted"
   "but someone else gets involved first"]
  "Twists for the narrative event generator.")

(defun solo-rpg--gen-nar-event-text ()
  "Generate and return a narative event text."
  (format "%s %s"
          (solo-rpg-table-get-random solo-rpg-gen-nar-event-incident-table)
          (solo-rpg-table-get-random solo-rpg-gen-nar-event-twist-table)))

(defun solo-rpg-gen-nar-event ()
  "Generate a random narrative event and stage it."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-nar-event-text))

;;; NPC Appearance generator:

(defun solo-rpg--gen-npc-body (mod)
  "Return random size affected by MOD."
  (let ((part (+ (+ 1 (random 4))
                 (+ 1 (random 4))
                 mod)))
    (cond ((< part 0) (setq part 1))
          ((> part 9) (setq part 9)))

    (nth (- part 1)
         '("extremely small"
           "very small"
           "small"
           "somewhat small"
           "average"
           "somewhat large"
           "large"
           "very large"
           "extremely large"))))

(defun solo-rpg--gen-npc-appearance-text ()
  "Generate and return NPC Appearance text."
  (let* ((height           (solo-rpg--table-weighted-get-random
                            solo-rpg-table-npc-height))
         (size             (solo-rpg--table-weighted-get-random
                            solo-rpg-table-npc-size))
         (eye-color        (solo-rpg--table-weighted-get-random
                            solo-rpg-table-npc-eye-color))
         (skin-color       (solo-rpg--table-weighted-get-random
                            solo-rpg-table-npc-skin-color))
         (hair-color       (if (string= skin-color "western")
                               (solo-rpg--table-weighted-get-random
                                solo-rpg-table-npc-hair-color)
                             "dark"))
         (hair-length      (solo-rpg--table-weighted-get-random
                            solo-rpg-table-npc-hair-length))
         (long-hair-style  (solo-rpg--table-weighted-get-random
                            solo-rpg-table-npc-long-hair-style))
         (facial-hair      (solo-rpg--table-weighted-get-random
                            solo-rpg-table-npc-facial-hair))
         (special-features (solo-rpg-table-get-random
                            solo-rpg-table-npc-special-features))
         (hair  (format "%s, %s" hair-color hair-length))
         (size-mod         (plist-get size :mod))
         (chest            (solo-rpg--gen-npc-body size-mod))
         (waist            (solo-rpg--gen-npc-body size-mod))
         (bottom           (solo-rpg--gen-npc-body size-mod))
         (output           ""))
    (unless (string= hair-length "short")
      (setq hair (format "%s, %s" hair long-hair-style)))

    (setq size (plist-get size :desc))
    
    (setq output (format "Height          : %s\n\
Size            : %s\n\
Eye color       : %s\n\
Skin color      : %s\n\
Hair            : %s\n\
Special features: %s\n"
                         height
                         size
                         eye-color
                         skin-color
                         hair
                         special-features))
    (when (eq solo-rpg-npc-facial-hair 'on)
      (setq output (concat output
                           (format "Facial hair     : %s\n"
                           facial-hair))))
    (when (eq solo-rpg-npc-nsfw 'on)
      (setq output (concat output
                           (format "Chest           : %s\n\
Waist           : %s\n\
Bottom          : %s\n"
                           chest waist bottom))))
    output))

(defun solo-rpg-gen-npc-appearance ()
  "Generate NPC appearance and open it in the staging area."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-npc-appearance-text))


;;; NPC Name generator:

(defconst solo-rpg-gen-npc-name-female-first-table
  '["En"
    "For"
    "Del"
    "Ha"
    "Sen"
    "Flo"
    "Van"
    "Ren"
    "Le"
    "Lo"]
  "First name part data for the female NPC name generator.")

(defconst solo-rpg-gen-npc-name-female-second-table
  '["na"
    "neva"
    "novus"
    "rama"
    "sinni"
    "bava"
    "sani"
    "vanna"]
  "Second name part for the female NPC name generator.")

(defun solo-rpg--gen-npc-name-female-text ()
  "Generate and return a random female NPC name."
  (format "%s%s"
          (solo-rpg-table-get-random solo-rpg-gen-npc-name-female-first-table)
          (solo-rpg-table-get-random solo-rpg-gen-npc-name-female-second-table)))

(defun solo-rpg-gen-npc-name-female ()
  "Generate a random female NPC name and stage it."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-npc-name-female-text))

(defconst solo-rpg-gen-npc-name-male-first-table
  '["Den"
    "Thar"
    "Han"
    "Mene"
    "Dar"
    "Thun"
    "Mana"
    "Hen"
    "Var"
    "Me"
    "Ho"
    "Leno"]
  "First name part data for the male NPC name generator.")

(defconst solo-rpg-gen-npc-name-male-second-table
  '["lo"
    "fa"
    "di"
    "na"
    "ren"
    "fla"
    "fo"]
  "Second name part data for the male NPC name generator.")

(defconst solo-rpg-gen-npc-name-male-last-table
  '["gan"
    "der"
    "deran"
    "then"
    "dor"
    "menor"
    "lanaz"
    "gor"
    "fex"
    "drin"
    "ger"]
  "Second name part for the male NPC name generator.")

(defun solo-rpg--gen-npc-name-male-text ()
  "Generate and return a random male NPC name."
  (if (= (random 2) 0)
      (format "%s%s"
              (solo-rpg-table-get-random
               solo-rpg-gen-npc-name-male-first-table)
              (solo-rpg-table-get-random
               solo-rpg-gen-npc-name-male-last-table))
    (format "%s%s%s"
            (solo-rpg-table-get-random
             solo-rpg-gen-npc-name-male-first-table)
            (solo-rpg-table-get-random
             solo-rpg-gen-npc-name-male-second-table)
            (solo-rpg-table-get-random
             solo-rpg-gen-npc-name-male-last-table))))

(defun solo-rpg-gen-npc-name-male ()
  "Generate a random male NPC name and stage it."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-npc-name-male-text))

;;; Dungeon room generator

(defconst solo-rpg-gen-dungeon-room-size-table
  '((2  . "small")
    (5  . "average size")
    (7  . "large")
    (8  . "very large"))
  "Room size data for the Dungeon Room generator.")

(defconst solo-rpg-gen-dungeon-room-shape-table
  '((5  . "rectangular")
    (7  . "square")
    (8  . "T-shaped")
    (9  . "L-shaped")
    (10 . "octagonal"))
  "Room shape data for the Dungeon Room generator.")

(defconst solo-rpg-dungeon-room-descs
  '(("storage room" .
     ((", filled with crates" . nil)
      (", filled with crates and chests" . nil)
      (", with a few chests along the walls" . nil)))
    ("ceremonial chamber" .
     ((" with a ritual circle in the middle" . nil)
      (" with a bottomless pit at its center" . nil)
      (" with a foreboding altar at its center" . nil)))
    ("burial chamber" .
     ((" with tombs along the walls" .
       ((", some of which are cracked open" .
         ((", seemingly from the inside" . nil)
          (", their slabs lying broken on the floor" . nil)))))
      (" with a central tomb" . nil)))
    ("room with blood stains on the floor" .
     ((", and cages along the walls" . nil)
      (", and raised platforms with seating on all sides" . nil)
      (", and pillars along the walls" . nil)))
    ("study" .
     ((" with book cases along the walls" . nil)
      (" with a desk at the center" .
       ((", surrounded by book cases" . nil)
        (", and a thick carpet covering the floor" . nil)))
      (" with the remains of a desk at its center" . nil))))
  "Data for all room types.")


(defun solo-rpg-gen-desc (options)
  "Recursive function for generating descriptions based on OPTIONS."
  (let ((chosen (nth (random (length options)) options)))
    (concat (car chosen)
            (if (null (cdr chosen))
                ""
              (solo-rpg-gen-desc (cdr chosen))))))

(defun solo-rpg-gen-dungeon-room-text ()
  "Return text describing a dungeon room."
  (let* ((room-data (solo-rpg--table-weighted-get-random-list "gen-dungeon-room"
                                                              '("size" "shape")))
         (room-desc     (solo-rpg-gen-desc solo-rpg-dungeon-room-descs))
         (exit-probs    (alist-get solo-rpg-dungeon-size
                                   solo-rpg-dungeon-room-exit-probs))
         (forward-exit  (<= (+ 1 (random 100)) (alist-get 'forward exit-probs)))
         (center-exit   (<= (+ 1 (random 100)) (alist-get 'center  exit-probs)))
         (away-exit     (<= (+ 1 (random 100)) (alist-get 'away    exit-probs)))
         (down-exit     (<= (+ 1 (random 100)) (alist-get 'down    exit-probs)))
         (exits         nil)
         (exits-text    "")
         (geometry-text ""))
    (when down-exit
      (push "down" exits))
    (when away-exit
      (push "away from center" exits))
    (when center-exit
      (push "towards center" exits))
    (when forward-exit
      (push "straight ahead" exits))
    (setq exits-text (if exits
                         (string-join exits ", ")
                       "dead end"))
    (setq geometry-text (let-alist room-data
                          (format "%s %s" .size .shape)))
    (concat (string-join (list
                          (format "Dungeon room: %s %s." geometry-text room-desc)
                          (format "Exits       : %s." exits-text))
                         "\n")
            "\n")))

(defun solo-rpg-gen-dungeon-room ()
  "Command for returning a dungeon room for staging."
  (interactive)
  (solo-rpg--stage #'solo-rpg-gen-dungeon-room-text))
;;; Dungeon event generator

(defconst solo-rpg-gen-dungeon-event-incident-table
  ["Sounds of movement echo through the halls"
   "You think you saw movement in the corner of your eye"
   "Someone - or something - appear to be approaching"
   "A sudden cave in alters the layout of the surroundings"
   "There is a flutter of movement"
   "There is a sudden draft"]
  "Incidents for the random dungeon event generator.")

(defconst solo-rpg-gen-dungeon-event-twist-table
  ["and a fleeting tactical advantage is revealed"
   "and there is a sudden, strange smell in the air"
   "but then the dungeon turns eerily quiet"
   "and things might be taking a turn for the worse"
   "but all may not be what they seem"
   "and an unexpected opportunity presents itself"]
  "Twists for the random dungeon event generator.")

(defun solo-rpg--gen-dungeon-event-text ()
  "Generate and return a random dungeon event text."
  (format "%s %s"
          (solo-rpg-table-get-random solo-rpg-gen-dungeon-event-incident-table)
          (solo-rpg-table-get-random solo-rpg-gen-dungeon-event-twist-table)))

(defun solo-rpg-gen-dungeon-event ()
  "Generate a random dungeon event and stage it."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-dungeon-event-text))
;;; Settlement event generator

(defconst solo-rpg-gen-city-event-incident-table
  ["Two factions collide"
   "A crime occurs in the street"
   "A chase breaks out"
   "There is a tussle in the street"
   "A suspicious figure darts into a nearby hiding place"
   "A local authority figure makes a loud proclamation"
   "A runaway draft animal or cart causes chaos"
   "Someone approaches to offer a dangerous secret"
   "A panicked citizen runs past, screaming for help"
   "Someone is accused of a crime"
   "A faction is loudly recruiting in the street"
   "A procession or parade is blocking the street"]
  "Incidents for the city event generator.")

(defconst solo-rpg-gen-city-event-twist-table
  ["but the outcome is unclear"
   "and people are gathering"
   "and it has attracted the attention of the guards"
   "and the guards appear uninterested"
   "and the crowd turns aggressive"
   "and in the commotion, an opportunity arises"
   "and things seem to be getting worse"
   "and something of great value is threatened"
   "but things may not be what they appear to be"
   "but the motives of those involved is questioned by someone"
   "and the city guards show up in force"
   "but someone unexpectedly intervenes"
   "but the situation is abruptly interrupted by a new threat"]
  "Twists for the city event generator.")

(defun solo-rpg--gen-city-event-text ()
  "Generate and return a city event text."
  (format "%s %s"
          (solo-rpg-table-get-random solo-rpg-gen-city-event-incident-table)
          (solo-rpg-table-get-random solo-rpg-gen-city-event-twist-table)))

(defun solo-rpg-gen-city-event ()
  "Generate a random city event and stage it."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-city-event-text))

;;; Wilderness event generator

(defconst solo-rpg-gen-wild-event-incident-table
  ["Something tells you that you are being stalked"
   "You come across a building or structure"
   "You hear cries for help in the distance"
   "The sounds of battle can be heard nearby"
   "Your way is blocked"
   "There is an unexpected change in the weather"
   "You meet someone"
   "A sudden, seeminly unnatrual slience befalls the area"
   "A local beast or creature crosses your path"
   "You spot an unusual landmark or natural formation"
   "You come across someone who asks for your help"]
  "Incidents for the wilderness event generator.")

(defconst solo-rpg-gen-wild-event-twist-table
  ["but all may not be what it seems"
   "and an opportunity presents itself"
   "and things may be about to get dangerous"
   "and a new danger is revealed"
   "and a hidden path, shortcut, or advantage is revealed"
   "and it leads to a rare foraging or hunting opportunity"
   "and a secret is revealed"
   "from which an advantage could be gained"
   "but something appears to be off"]
  "Twists for the wilderness event generator.")

(defun solo-rpg--gen-wild-event-text ()
  "Generate and return a wilderness event text."
  (format "%s %s"
          (solo-rpg-table-get-random solo-rpg-gen-wild-event-incident-table)
          (solo-rpg-table-get-random solo-rpg-gen-wild-event-twist-table)))

(defun solo-rpg-gen-wild-event ()
  "Generate a random wilderness event and stage it."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-wild-event-text))

;;; Weather generator

(defconst solo-rpg-gen-weather-table
  '((1  . (:desc "Thunder or snowstorm" :mod -2))
    (3  . (:desc "Heavy rain or snow"   :mod -1))
    (4  . (:desc "Rain or snow"         :mod  0))
    (6  . (:desc "Overcast"             :mod  0))
    (9  . (:desc "Scattered clouds"     :mod  0))
    (11 . (:desc "Nearly clear skies"   :mod  1))
    (12 . (:desc "Clear skies"          :mod  2)))
  "Weather types for the weather generator.")

(defun solo-rpg--gen-weather-text ()
  "Generate and return weather text."
  (let* ((weather      (solo-rpg--table-weighted-get-random
                        solo-rpg-gen-weather-table))
         (weather-desc (plist-get weather :desc))
         (weather-mod  (plist-get weather :mod))
         (temp         (+ (random 4)
                          (random 4)
                          weather-mod)))
    (cond ((< temp 0) (setq temp 0))
          ((> temp 6) (setq temp 6)))
    (format "%s, %s"
            weather-desc
            (nth temp
                 '("far colder than usual"
                   "colder than usual"
                   "a bit colder than usual"
                   "temperature as expected for the season"
                   "a bit warmer than usual"
                   "warmer than usual"
                   "far warmer than usual")))))

(defun solo-rpg-gen-weather ()
  "Generate random weather and stage it."
  (interactive)
  (solo-rpg--stage #'solo-rpg--gen-weather-text))
  
;;; LONELOG:
;;; Tag handling:

(defun solo-rpg-extract-latest-tags ()
  "Scan the buffer backwards to extract the latest state of each tag.
Returns a chronologically ordered list of tag strings."
  ;; 1. Save the user's cursor position so we don't yank their screen around.
  (save-excursion
    ;; 2. Jump to the absolute bottom of the document.
    (goto-char (point-max))

    ;; 3. Set up our temporary variables for this run.
    (let ((seen-ids (make-hash-table :test 'equal))
          (latest-tags nil)
          ;; Regex: Group 1 matches anything that isn't a ], [, or |
          (tag-regex "\\[\\([^][|]+\\)[^][]*\\]"))

      ;; 4. Loop backwards until we run out of matches.
      (while (re-search-backward tag-regex nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          
          ;; 5. Check if it's wrapped in double brackets (like an Org link).
          ;; If it is, we completely ignore it.
          (unless (or (and (> start (point-min)) (eq (char-before start) ?\[))
                      (and (< end (point-max)) (eq (char-after end) ?\])))
            
            (let ((full-tag (match-string-no-properties 0))
                  (tag-id   (match-string-no-properties 1)))
              
              ;; 6. Have we seen this ID before?
              (unless (gethash tag-id seen-ids)
                ;; No? Then this is the newest version.
                ;; Mark it as seen in the hash table.
                (puthash tag-id t seen-ids)
                ;; Add the full tag to the front of our list.
                (push full-tag latest-tags))))))

      ;; 7. Return the finalized list.
      latest-tags)))

(defun solo-rpg-toggle-hud ()
  "Toggle the visibility of the Solo-RPG HUD side-window."
  (interactive)
  (let ((hud-win (solo-rpg--get-visible-hud-window)))
    (if hud-win
        (delete-window hud-win)
      (solo-rpg-update-hud))))

(defun solo-rpg--any-active-sessions-p (&optional ignore-buf)
  "Return t if there are live game buffers, ignoring IGNORE-BUF."
  (seq-some (lambda (buf)
              (and (not (eq buf ignore-buf)) ; Ignore the dying buffer
                   (buffer-local-value 'solo-rpg-mode buf)
                   (not (string-match-p "^\\*Solo-RPG HUD"
                                        (buffer-name buf)))))
            (buffer-list)))

(defun solo-rpg--cleanup-hud-if-last (&optional ignore-buf)
  "Close the HUD window and kill HUD buffers if no solo-rpg sessions remain.
IGNORE-BUF is ignored in the tally."
  (unless (solo-rpg--any-active-sessions-p ignore-buf)
    ;; 1. Close the window if it's currently on screen
    (let ((hud-win (solo-rpg--get-visible-hud-window)))
      (when hud-win
        (delete-window hud-win)))
    ;; 2. Silently assassinate all orphaned HUD buffers
    (dolist (buf (buffer-list))
      (when (string-match-p "^\\*Solo-RPG HUD" (buffer-name buf))
        (kill-buffer buf)))))

(defun solo-rpg--cleanup-on-kill ()
  "Hook function to clean up the HUD, ignoring the dying buffer."
  (solo-rpg--cleanup-hud-if-last (current-buffer)))

(defun solo-rpg--get-visible-hud-window ()
  "Return the window displaying a Solo-RPG HUD, if one exists."
  (seq-find (lambda (win)
               (string-match-p "^\\*Solo-RPG HUD"
                               (buffer-name (window-buffer win))))
             (window-list)))

(defun solo-rpg--swap-hud-on-window-change (&optional _)
  "Swap the HUD buffer to match the active game, if a HUD is open."
  (when (and solo-rpg-mode
             solo-rpg--hud-buffer-name
             (not (string-match-p "^\\*Solo-RPG HUD" (buffer-name))))
    (let ((hud-buf (get-buffer solo-rpg--hud-buffer-name))
          (visible-hud-win (solo-rpg--get-visible-hud-window)))
      ;; If our HUD exists, AND a HUD window is open on screen...
      (when (and hud-buf visible-hud-win
                 ;; ... and the window isn't ALREADY showing our HUD
                 (not (eq (window-buffer visible-hud-win) hud-buf)))
        ;; Lightning-fast swap: just change the text in that exact window
        (set-window-buffer visible-hud-win hud-buf)))))

(defun solo-rpg--draw-hud-contents (hud-buffer tags)
  "Wipe HUD-BUFFER and cleanly insert TAGS."
  (with-current-buffer hud-buffer
    ;; Save the user's cursor in case they actually clicked inside the HUD
    (save-excursion
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'text-mode)
          (text-mode))
        (erase-buffer)
        (insert "=== Active Tags ===\n\n")
        (if tags
            (dolist (tag tags)
              (insert tag "\n"))
          (insert "Any [tags] will be shown here.\n"))
        (unless solo-rpg-mode
          (solo-rpg-mode 1))))))

(defun solo-rpg-update-hud ()
  "Extract the latest tags and pop open the dedicated side-window HUD."
  (interactive)
  ;; If we don't have a unique HUD name for this buffer yet, make one!
  (unless solo-rpg--hud-buffer-name
    (setq solo-rpg--hud-buffer-name (format "*Solo-RPG HUD: %s*" (buffer-name))))
  (let ((tags (solo-rpg-extract-latest-tags))
        (hud-buffer (get-buffer-create solo-rpg--hud-buffer-name)))
    (solo-rpg--draw-hud-contents hud-buffer tags)
    (display-buffer hud-buffer
                    `(display-buffer-in-side-window
                      . ((side . right)
                         (window-width . ,solo-rpg-hud-width))))))

(defun solo-rpg--update-hud-background (source-buffer)
  "Silently update the HUD for SOURCE-BUFFER, but only if it's visible."
  ;; 1. Make sure the user hasn't closed the game buffer.
  (when (buffer-live-p source-buffer)
    ;; 2. Fetch the unique HUD name for this specific game
    (let ((hud-name (buffer-local-value 'solo-rpg--hud-buffer-name
                                        source-buffer)))
      ;; 3. If the HUD is currently open on the screen...
      (when (and hud-name (get-buffer-window hud-name))
        ;; 4. ... teleport into the game buffer to do the scanning
        (with-current-buffer source-buffer
          (let ((tags (solo-rpg-extract-latest-tags))
                (hud-buffer (get-buffer-create hud-name)))
            ;; 5. Draw the results
            (solo-rpg--draw-hud-contents hud-buffer tags)))))))

;;; Output functions:
;;; - Scene:

(defun solo-rpg-scene-start (scene-title)
  "Ask for SCENE-TITLE, insert a Solo-RPG scene heading in the current buffer."
  (interactive "sEnter new scene title: ")
  (let ((prev-scene-num 0)
        (scene-search-string "^S\\([0-9]+\\) \\*"))
    (save-excursion
      (when (re-search-backward scene-search-string nil t)
        (setq prev-scene-num (string-to-number (or (match-string 1) "0")))))
    (insert (format "S%d *%s*" (+ 1 prev-scene-num) scene-title))))

;;; DASHBOARDS:
;;; Dice dashboard:

(transient-define-prefix solo-rpg-menu-dice ()
  "The solo-rpg Dice menu."
  ["Solo-PRG dashboard: Dice Menu"
   ["Actions"
    ("r" "Roll dice"   solo-rpg-dice-roll-cast)
    ("q" "Go back"     transient-quit-one)]])

;;; Oracle dashboards:

;; These lambda functions should be their own defuns to make Flymake happy.
;; And they should also be generated by a macro. One day, when my skills with
;; elisp improve, I might fix that. "But that is not this day" - Aragorn
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

;;; Tarot dashboard:

;; Define the Tarot dashboard menu

(transient-define-prefix solo-rpg-menu-tarot ()
  "The solo-rpg Tarot menu."
  ["Solo-PRG dashboard: Tarot Menu"
   ["Draw"
    ("s" "Single tarot card"    solo-rpg-deck-tarot-draw-single
     :transient t)]
   ["System"
    ("q" "Go back"              transient-quit-one)]])


;;; Narrative dashboard:

;; Define the Plot dashboard menu

(transient-define-prefix solo-rpg-menu-nar ()
  "The solo-rpg Narrative menu."
  ["SoloRPG dashboard: Narrative Menu\n"
   ["Generate"
    ("n" "Narrative event" solo-rpg-gen-nar-event)
    ("p" "Plot"            solo-rpg-gen-plot)]
   ["System"
    ("q" "Go back"         transient-quit-one)]])

;;; NPC dashboard:

;; Define the NPC dashboard menu

(transient-define-prefix solo-rpg-menu-npc ()
  "The solo-rpg NPC menu."
  ["SoloRPG dashboard: NPC Menu\n"
   ["Generate"
    ("a" "Appearance"        solo-rpg-gen-npc-appearance)
    ("f" "Female name"       solo-rpg-gen-npc-name-female)
    ("m" "Male name"         solo-rpg-gen-npc-name-male)
    ("h" solo-rpg-toggle-npc-facial-hair
     :description solo-rpg--toggle-npc-facial-hair-desc
     :transient t)
    ("x" solo-rpg-toggle-npc-nsfw
     :description solo-rpg--toggle-npc-nsfw-desc
     :transient t)]
   ["System"
    ("q" "Go back"               transient-quit-one)]])

;;; Environment dashboards:

;; Dungeon dashboard

(transient-define-prefix solo-rpg-menu-dungeon ()
  "The solo-rpg Dungeon menu."
  ["SoloRPG dashboard: Dungeon Menu\n"
   ["Generate"
    ("r" "Room"            solo-rpg-gen-dungeon-room)
    ("s" solo-rpg-toggle-dungeon-size
     :description solo-rpg--toggle-dungeon-size-desc
     :transient t)
    ("e" "Event"           solo-rpg-gen-dungeon-event)
    ("n" "Narrative event" solo-rpg-gen-nar-event)]
   ["System"
    ("q" "Go back"         transient-quit-one)]])

;; Settlement dashboards

(transient-define-prefix solo-rpg-menu-city ()
  "The solo-rpg Settlement menu."
  ["SoloRPG dashboard: Settlement Menu\n"
   ["Generate"
    ("c" "City event"      solo-rpg-gen-city-event)
    ("n" "Narrative event" solo-rpg-gen-nar-event)
    ("w" "Weather"         solo-rpg-gen-weather)]
   ["System"
    ("q" "Go back"         transient-quit-one)]])

;; Travel dashboard

(transient-define-prefix solo-rpg-menu-travel ()
  "The solo-rpg Travel menu."
  ["SoloRPG dashboard: Travel Menu\n"
   ["Generate"
    ("e" "Wilderness event"  solo-rpg-gen-wild-event)
    ("n" "Narrative event"   solo-rpg-gen-nar-event)
    ("w" "Weather"           solo-rpg-gen-weather)]
   ["System"
    ("q" "Go back"           transient-quit-one)]])

;;; Main menu dashboard:

;; Define the main dashboard menu
;;;###autoload
(transient-define-prefix solo-rpg-menu ()
  "The main solo-rpg menu."
  ["SoloRPG dashboard: Main Menu\n"
   ["Misc"
    ("d" "Dice..."       solo-rpg-menu-dice)
    ("n" "NPCs..."       solo-rpg-menu-npc)
    ("a" "Narrative..."  solo-rpg-menu-nar)]
   ["Questions"
    ("o" "Oracles..."    solo-rpg-menu-oracle)
    ("t" "Tarot..."      solo-rpg-menu-tarot)]
   ["Environments"
    ("D" "Dungeons..."   solo-rpg-menu-dungeon)
    ("S" "Settlements..." solo-rpg-menu-city)
    ("T" "Travel..."     solo-rpg-menu-travel)]
   ["System"
    ("h" "Toggle HUD"    solo-rpg-toggle-hud
     :transient t)
    ("O" solo-rpg-output-method-toggle
     :description solo-rpg--toggle-output-desc
     :transient t)
    ("q" "Quit"          transient-quit-one)]])


;;; FACES:

;; The Macro Definition
(defmacro solo-rpg-define-face (name dark-hex light-hex docstring &optional bold)
  "Define a Solo-RPG face with NAME, using DARK-HEX and LIGHT-HEX colors.
DOCSTRING provides the documentation for the face.
If BOLD is non-nil, the face will be bold in all themes."
  (let ((weight-spec (if bold '(:weight bold) '())))
    `(defface ,name
       '(
         ;; Dark Background
         (((class color) (background dark))
          :foreground ,dark-hex ,@weight-spec)
         ;; Light Background
         (((class color) (background light))
          :foreground ,light-hex ,@weight-spec)
         ;; Fallback (Terminal / Monochrome)
         (t ,@weight-spec))
       ,docstring
       :group 'solo-rpg)))

;; --- Face Definitions ---

;; Action (@)
(solo-rpg-define-face solo-rpg-action-symbol-face
  "#045ccf" "#003f91"
  "Foreground color for the Solo-RPG action symbol (the \"@\")."
  t) ;; Bold

(solo-rpg-define-face solo-rpg-action-content-face
  "#a3cbff" "#1e4e8c"
  "Foreground color for the Solo-RPG action.
This is the part that comes after the \"@\".")

;; Oracle (?)
(solo-rpg-define-face solo-rpg-oracle-question-symbol-face
  "#b020a0" "#6d207a"
  "Foreground color for the Solo-RPG oracle question symbol (the \"?\")."
  t) ;; Bold

(solo-rpg-define-face solo-rpg-oracle-question-content-face
  "#f490ec" "#5e3fd3"
  "Foreground color for the Solo-RPG oracle question itself.
This is the part that comes after the \"?\".")

;; Mechanics (d:)
(solo-rpg-define-face solo-rpg-mechanics-roll-symbol-face
  "#308018" "#2e7d12"
  "Foreground color for the Solo-RPG mechanics roll symbol (the \"d:\")."
  t) ;; Bold

(solo-rpg-define-face solo-rpg-mechanics-roll-content-face
  "#60ff28" "#206009"
  "Foreground color for the Solo-RPG mechanics roll itself.
This is the part that comes after the \"d:\".")

;; Result (->)
(solo-rpg-define-face solo-rpg-oracle-and-dice-result-symbol-face
  "#a09005" "#99a600"
  "Foreground color for the Solo-RPG oracle/dice symbol (the \"->\")."
  t) ;; Bold

(solo-rpg-define-face solo-rpg-oracle-and-dice-result-content-face
  "#e8fc05" "#708600"
  "Foreground color for the Solo-RPG oracle/dice result itself.
This is the part that comes after the \"->\".")

;; Consequence (=>)
(solo-rpg-define-face solo-rpg-consequence-symbol-face
  "#c04008" "#936400"
  "Foreground color for the Solo-RPG consequence symbol (the \"=>\")."
  t) ;; Bold

(solo-rpg-define-face solo-rpg-consequence-content-face
  "#ffa050" "#b37400"
  "Foreground color for the Solo-RPG consequence itself.
This is the part that comes after the \"=>\".")

;; Tags ([..:..|..])
(solo-rpg-define-face solo-rpg-tag-symbol-face
                     "#00ff00" "#00cc00"
                     "Foreground color for the Solo-RPG tag symbols themselves.
They are the `[' and `]' characters.")

(solo-rpg-define-face solo-rpg-tag-separator-face
                     "#00aa00" "#008800"
                     "Foreground color for the Solo-RPG tag separators (| and :).")

;; Face rules:

(defvar solo-rpg-font-lock-keywords
  (list
   ;; Action:
   '("^\\(@\\)\\s-*\\(.*\\)"
     (1 'solo-rpg-action-symbol-face)
     (2 'solo-rpg-action-content-face))
   ;; Oracle question:
   '("^\\(\\?\\)\\s-*\\(.*\\)"
     (1 'solo-rpg-oracle-question-symbol-face)
     (2 'solo-rpg-oracle-question-content-face))
   ;; Mechanics roll:
   '("^\\(d:\\)\\s-*\\(.*\\)"
     (1 'solo-rpg-mechanics-roll-symbol-face)
     (2 'solo-rpg-mechanics-roll-content-face))
   ;; Oracle and dice result:
   '("\\(->\\)\\s-*\\(.*\\)"
     (1 'solo-rpg-oracle-and-dice-result-symbol-face t)    ; t = Override
     (2 'solo-rpg-oracle-and-dice-result-content-face t)) ; t = Override
   ;; Consequence:
   '("\\(=>\\)\\s-*\\(.*\\)"
     (1 'solo-rpg-consequence-symbol-face t)     ; t = Override
     (2 'solo-rpg-consequence-content-face t)) ; t = Override
   ;; Tags (with anchored mini-search for | and :):
   '("\\(\\[\\)\\([^]]+\\)\\(\\]\\)"
     (1 'solo-rpg-tag-symbol-face t)
     (3 'solo-rpg-tag-symbol-face t)
     ;; --- The Anchored Matcher ---
     ("[|:]"
      ;; Pre-match form: jump to the start of the tag contents,
      ;; and tell Emacs to stop searching at the end of the tag contents.
      (progn (goto-char (match-beginning 2)) (match-end 2))
      ;; Post-match form: jump back to the end of the closing bracket
      ;; so Emacs can continue highlighting the rest of the file normally.
      (goto-char (match-end 0))
      ;; Subexp-highlighter: apply the face to the | or :
      (0 'solo-rpg-tag-separator-face t))))
  "Highlighting rules for Solo-RPG mode.")

;;; MINOR MODE:

;;;###autoload
(defvar solo-rpg-mode-map (make-sparse-keymap)
  "Keymap for `solo-rpg-mode'.
By default, this is empty to allow users to define their own menu key.")

;;;###autoload
(define-minor-mode solo-rpg-mode
  "Minor mode with support for Lonelog and playing solo roleplaying games.

When enabled, this mode provides syntax highlighting for the five core
Lonelog symbols:
 @   Action
 ?   Oracle
 d:  Mechanics roll
 ->  Result
 =>  Consequence

Tags are also tracked in a side window:
 [N:Jonah|friendly|Uninjured]

For more information about the Lonelog solo RPG notation format, see:
https://zeruhur.itch.io/lonelog

\\{solo-rpg-mode-map}"
  :init-value nil
  :global nil
  :group 'solo-rpg
  :lighter " SoloRPG"
  :keymap solo-rpg-mode-map

  (if solo-rpg-mode
      ;; If ON:
      (progn
        (font-lock-add-keywords nil solo-rpg-font-lock-keywords)
        (font-lock-flush)
        
        ;; Check if the buffer name starts with "*Solo-RPG HUD"
        (unless (string-match-p "^\\*Solo-RPG HUD" (buffer-name))
          ;; Generate the unique HUD name for this buffer
          (setq solo-rpg--hud-buffer-name (format "*Solo-RPG HUD: %s*"
                                                  (buffer-name)))
          ;; Start the timer, and hand it the current game buffer.
          (setq solo-rpg--hud-timer
                (run-with-idle-timer solo-rpg-hud-update-delay t
                                     #'solo-rpg--update-hud-background
                                     (current-buffer)))
          
          ;; Attach the cleanup check to this buffer's death event.
          ;; The last `t' makes it buffer-local.
          (add-hook 'kill-buffer-hook #'solo-rpg--cleanup-on-kill nil t)
          ;; Handle auto-start
          (when solo-rpg-auto-open-hud
            (solo-rpg-update-hud))
          
          (add-hook 'window-selection-change-functions
                    #'solo-rpg--swap-hud-on-window-change)
          
          ;; Initialize Tarot deck
          (setq solo-rpg-deck-tarot-active (solo-rpg-deck-tarot-copy)))
              
        (message "Solo-RPG-mode enabled."))
    ;; If OFF:
    (progn
      (font-lock-remove-keywords nil solo-rpg-font-lock-keywords)
      (font-lock-flush)
      ;; Stop the idle timer.
      (when solo-rpg--hud-timer
        (cancel-timer solo-rpg--hud-timer)
        (setq solo-rpg--hud-timer nil))
      
      ;; Remove our kill-buffer hook hook so it doesn't fire unnecessarily
      ;; The last `t' makes it buffer-local.
      (remove-hook 'kill-buffer-hook #'solo-rpg--cleanup-on-kill t)
      
      ;; Run the cleanup check
      (solo-rpg--cleanup-hud-if-last)
      
      (message "Solo-RPG-mode disabled."))))

(provide 'solo-rpg)

;;; solo-rpg.el ends here
