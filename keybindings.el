;; key chords
(use-package key-chord
  :custom
    (key-chord-two-keys-delay 0.3)
    (key-chord-one-key-delay 0.3)
  :config
    (key-chord-mode 1)
    ;; jk and kj return to normal state
    (key-chord-define evil-visual-state-map "kj" 'evil-normal-state)
    (key-chord-define evil-visual-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    ;; chords to avoid reaching to shift key
    (key-chord-define evil-insert-state-map "qq" "/")
    (key-chord-define evil-insert-state-map "ww" "?")
    (key-chord-define evil-insert-state-map "''" "\"")
    (key-chord-define evil-insert-state-map "11" "!")
    (key-chord-define evil-insert-state-map "22" "@")
    (key-chord-define evil-insert-state-map "33" "#")
    (key-chord-define evil-insert-state-map "44" "$")
    (key-chord-define evil-insert-state-map "55" "%")
    (key-chord-define evil-insert-state-map "77" "&")
    (key-chord-define evil-insert-state-map "88" "*")
    (key-chord-define evil-insert-state-map "99" "(")
    (key-chord-define evil-insert-state-map "00" ")")
    (key-chord-define evil-insert-state-map "--" "_")
    (key-chord-define evil-insert-state-map "==" "+")
    (key-chord-define evil-insert-state-map "[[" "{"))
