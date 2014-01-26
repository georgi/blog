Emacs Dark Theme
================

Recently I updated my Emacs setup and decided to go for a more minimal
color theme. Especially on long coding nights you want a theme without
distracting colors.

![Emacs dark theme][1]


### Configuration

Just paste this code into your emacs condiguration:

    
     (set-face-attribute 'default nil
                         :background "grey20"
                         :foreground "grey90")
      
     (set-face-attribute 'modeline nil
                         :background "grey10"
                         :foreground "grey90")
      
     (set-face-attribute 'cursor nil
                         :background "white")
      
     (set-face-attribute 'font-lock-builtin-face nil
                         :foreground "grey60")
      
     (set-face-attribute 'font-lock-comment-face nil
                         :foreground "grey60")
      
     (set-face-attribute 'font-lock-constant-face nil
                         :foreground "grey60")
      
     (set-face-attribute 'font-lock-keyword-face nil
                         :foreground "white")
      
     (set-face-attribute 'font-lock-string-face nil
                         :foreground "white")
      
     (set-face-attribute 'font-lock-variable-name-face nil
                         :foreground "lightblue")
      
     (set-face-attribute 'font-lock-function-name-face nil
                         :foreground "lightblue")
      
     (set-face-attribute 'region nil
                         :background "#111")                    


### Additional faces

If you use speedbar or elscreen, you can set additional faces:


    (set-face-attribute 'speedbar-file-face nil
                        :foreground "white")
     
    (set-face-attribute 'elscreen-tab-background-face nil
                        :background "grey10"
                        :foreground "grey90")
     
    (set-face-attribute 'elscreen-tab-control-face nil
                        :background "grey20"
                        :foreground "grey90")
     
    (set-face-attribute 'elscreen-tab-current-screen-face nil
                        :background "grey20"
                        :foreground "grey90")
     
    (set-face-attribute 'elscreen-tab-other-screen-face nil
                        :background "grey30"
                        :foreground "grey60")



[1]: http://www.matthias-georgi.de/images/emacs-dark-theme.png
