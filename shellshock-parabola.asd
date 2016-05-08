;;;; shellshock-parabola.asd

(asdf:defsystem #:shellshock-parabola
  :description "Describe shellshock-parabola here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl
               #:temporal-functions
               #:cepl.sdl2
               #:swank
               #:livesupport
               #:cepl.skitter.sdl2
               #:cepl.devil)
  :serial t
  :components ((:file "package")
               (:file "shellshock-parabola")))

