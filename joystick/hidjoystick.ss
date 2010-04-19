#lang scheme

(require scheme/foreign)
(unsafe!)

(define libIOKit (ffi-lib "libIOKit"))
(define-syntax-rule (define-IO obj typ)
  (define obj (get-ffi-obj 'obj libIOKit typ)))
(define CFlib (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))
(define-syntax-rule (define-CF obj typ)
  (define obj (get-ffi-obj 'obj CFlib typ)))

(define-cpointer-type _mach_port_t)
(define _kern_return_t _int)
(define-cpointer-type _CFMutableDictionaryRef)
(define-cpointer-type _CFAllocatorRef)
(define-cpointer-type _CFNumberRef)
(define _CFNumberType
  (_enum 
   '(kCFNumberSInt8Type = 1
                        kCFNumberSInt16Type = 2
                        kCFNumberSInt32Type = 3
                        kCFNumberSInt64Type = 4
                        kCFNumberFloat32Type = 5
                        kCFNumberFloat64Type = 6
                        kCFNumberCharType = 7
                        kCFNumberShortType = 8
                        kCFNumberIntType = 9
                        kCFNumberLongType = 10
                        kCFNumberLongLongType = 11
                        kCFNumberFloatType = 12
                        kCFNumberDoubleType = 13
                        kCFNumberCFIndexType = 14
                        kCFNumberNSIntegerType = 15
                        kCFNumberCGFloatType = 16
                        kCFNumberMaxType = 16)))
(define-cpointer-type _CFStringRef)
(define _CFStringBuiltInEncodings
  (_enum
   '(kCFStringEncodingMacRoman = 0
                               kCFStringEncodingWindowsLatin1 = #x0500
                               kCFStringEncodingISOLatin1 = #x0201
                               kCFStringEncodingNextStepLatin = #x0B01
                               kCFStringEncodingASCII = #x0600
                               kCFStringEncodingUnicode = #x0100
                               kCFStringEncodingUTF8 = #x08000100
                               kCFStringEncodingNonLossyASCII = #x0BFF
                               
                               kCFStringEncodingUTF16 = #x0100
                               kCFStringEncodingUTF16BE = #x10000100
                               kCFStringEncodingUTF16LE = #x14000100
                               kCFStringEncodingUTF32 = #x0c000100
                               kCFStringEncodingUTF32BE = #x18000100
                               kCFStringEncodingUTF32LE = #x1c000100)))
(define _CFStringEncoding _CFStringBuiltInEncodings)

(define-IO IOMasterPort (_fun (_mach_port_t/null = #f) (mport : (_ptr o _mach_port_t))
                              -> (code : _kern_return_t)
                              -> (values code mport)))
(define-IO IOServiceMatching (_fun _string -> _CFMutableDictionaryRef))
(define kIOHIDDeviceKey "IOHIDDevice")
(define-IO CFNumberCreate (_fun (_CFAllocatorRef/null = #f) _CFNumberType (valuePtr : (_ptr i _int))
                                -> (ref : _CFNumberRef)
                                -> ref))
(define kHIDUsage_GD_GamePad #x05)
(define kHIDPage_GenericDesktop #x01)
(define-IO CFDictionarySetValue (_fun _CFMutableDictionaryRef _pointer _pointer -> _void))
(define kIOHIDPrimaryUsageKey "PrimaryUsage")
(define kIOHIDPrimaryUsagePageKey "PrimaryUsagePage")
(define-IO CFStringCreateWithCString (_fun (_CFAllocatorRef/null = #f) _string (_CFStringEncoding = 'kCFStringEncodingASCII)
                                           -> _CFStringRef))
(define-IO CFRelease (_fun _pointer -> _void))

(define nType kHIDUsage_GD_GamePad)
(define nClass kHIDPage_GenericDesktop)
(define-values (code mport) (IOMasterPort))
(define pDictionary (IOServiceMatching kIOHIDDeviceKey))

(define (CFDictionarySetValue-Int pDictionary str n)
  (define ref (CFNumberCreate 'kCFNumberIntType n))
  (CFDictionarySetValue pDictionary (CFStringCreateWithCString str) ref)
  (CFRelease ref))

(CFDictionarySetValue-Int pDictionary kIOHIDPrimaryUsageKey nType)
(CFDictionarySetValue-Int pDictionary kIOHIDPrimaryUsagePageKey nClass)