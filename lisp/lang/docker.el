(use-package docker)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(general-define-key
 :states  '(normal)
 :keymaps '(docker-compose-mode-map dockerfile-mode-map)
 :prefix  "SPC m"
 "d" 'docker)

(general-define-key
 :states  '(normal)
 :keymaps '(docker-image-mode-map
            docker-volume-mode-map
            docker-machine-mode-map
            docker-network-mode-map
            docker-container-mode-map)
 :prefix  "SPC m"
 "c" 'docker-container-help
 "i" 'docker-image-help
 "m" 'docker-machine-help
 "n" 'docker-network-help
 "v" 'docker-volume-help)


(general-define-key
 :states  '(normal)
 :keymaps '(docker-image-mode-map
            docker-volume-mode-map
            docker-machine-mode-map
            docker-network-mode-map
            docker-container-mode-map)
 "M" 'tablist-unmark-forward
 "m" 'tablist-mark-forward)
