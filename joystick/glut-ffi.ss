#lang scheme

(require scheme/foreign)
(unsafe!)

(define libglut (ffi-lib "/System/Library/Frameworks/GLUT.framework/Versions/A/GLUT"))
(define-syntax-rule (define-ffi obj typ)
  (define obj (get-ffi-obj 'obj libglut typ)))

(define-ffi glutJoystickFunc
  (_fun (_fun _uint _int _int _int -> _void)
        _int
        -> _void))

(provide glutJoystickFunc)