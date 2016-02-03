!------ default parameters

      integer   NOT_SET_I
      integer   DEF_VAL_I
      integer   DEFERRED_I

      real      DEF_VAL_R
      real      NOT_SET_R
      real      DEFERRED_R

      character*64 NOT_SET_C

      parameter (DEF_VAL_I =  (2**16-1))
      parameter (NOT_SET_I = -DEF_VAL_I)
      parameter (DEFERRED_I = DEF_VAL_I - 1)

      parameter (DEF_VAL_R  =  1.0e+36)
      parameter (NOT_SET_R  = -DEF_VAL_R)
      parameter (DEFERRED_R = 2.*DEF_VAL_R)

      parameter (NOT_SET_C = 'NOT SET')
