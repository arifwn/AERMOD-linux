      SUBROUTINE SIGY( XARG )
C***********************************************************************
C             SIGY Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates ambient sigma-Y values
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        REVISIONS:  Modified to incorporate meander algorithm
C                    as per Venky's memo of 6/24/98
C                    R. Paine, ENSR, 6/26/98
C
C                    Modified to use OPTB1 for HS .GT. 30m and
C                    OPTB2 for HS .LE. 30m.
C                    R. Brode, PES, 7/14/95
C
C                    Added Developmental Options, OPTB1 and OPTB2
C                    R. Brode, PES, 1/6/95
C
C        INPUTS:  Downwind distance, XARG, in meters
C                 Effective wind speed and sigma_V, UEFF and SVEFF
C
C        OUTPUTS: Ambient Lateral Dispersion Coefficient, SYAMB
C
C        CALLED FROM:   PDIS
C                       VDIS (when volume sources are implemented)
C                       ADIS (when area sources are implemnted)
C
C        Assumptions:
C
C        References:  "Global Organizations and Linkage of Formulas",
C                      7/30/93
C                     "A Dispersion Model for the Convective
C                      Boundary Layer (Revised)", J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION, PARAMETER :: EXPON = 1.0D0
      DOUBLE PRECISION :: XARG, SVOVRU, TYEFF, 
     &         XNODIM, DENOMI, BETALPH, TYEFF3

C     Variable Initializations
      MODNAM = 'SIGY'

      IF( STABLE .OR. (UNSTAB .AND. (HS .GE. ZI)) )THEN
C        The atmosphere is stable or the release is above the CBL mixing ht.
C        Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
         SVOVRU = MAX (SVUMIN, SVEFF/UEFF)
         TYEFF  = (ZIMECH/(156.0D0*SVEFF)) * (MAX(HE, 0.46D0)/0.46D0)
         SYAMB  = (SVOVRU * XARG)/(1.0D0+XARG/(2.0D0*UEFF*TYEFF))**0.3D0

      ELSEIF( UNSTAB )then
C        The atmosphere is unstable and the release is below the CBL mixing ht.
C        Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
         SVOVRU = MAX (SVUMIN, SVEFFD/UEFFD)
         XNODIM = SVOVRU * XARG / ZI
         DENOMI  = MAX (HS, 0.46D0)
C ---    BETALPH= MAX( 78.0D0*(0.46D0/DENOMI)**EXPON , 0.7D0)
C ---             recode without EXPON, since EXPON=1
         BETALPH= MAX( 78.0D0*(0.46D0/DENOMI), 0.7D0)
         SYAMB  = (SVOVRU * XARG) / (1.0D0+BETALPH*XNODIM)**0.3D0

C        Assign ambient value for indirect source = direct source
         SYAN   = SYAMB

C        Calculate the ambient sigma_Y for a penetrated plume, SYA3
         IF( PPF .GT. 0.0D0 )THEN
C           Calculate SV/U, but not less than PARAMETER, SVUMIN = 0.05
            SVOVRU = MAX (SVUMIN, SVEFF3/UEFF3)
            TYEFF3 = (ZIMECH/(156.0D0*SVEFF3))*(MAX(HE3,0.46D0)/0.46D0)
            SYA3   = (SVOVRU*XARG)/
     &                         (1.0D0+XARG/(2.0D0*UEFF3*TYEFF3))**0.3D0

         ELSE
            SYA3 = 0.0D0
         ENDIF

      ENDIF

      RETURN
      END

      SUBROUTINE SIGZ( XARG )
C***********************************************************************
C             SIGZ Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates ambient sigma-z values
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        REVISIONS:  Modified to use sigma-z formulation from CTDMPLUS
C                    for stable and penetrated plumes.
C                    R. Brode, PES, 8/5/98
C
C                    Added check to sign of TGEFF before calculation
C                    of BVFRQ.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C
C        INPUTS:  Downwind Distance, XARG (in meters)
C                 Effective wind speed, UEFF, and sigma_W, SWEFF
C
C        OUTPUTS: Vertical Dispersion Coefficient, SZ
C
C        CALLED FROM:   PDIS
C                       VDIS (when volume sources are implemented)
C                       ADIS (when area sources are implemented)
C
C        Assumptions:
C
C        References:    "Dispersion in the Stable Boundary Layer",
C                        A. Venkatram, 2/12/93
C                       "A Dispersion Model for the Convective Boundary
C                        Layer (Revised)", J. Weil, 8/17/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: NDXZHE
      DOUBLE PRECISION :: XARG, TTRAVL, PTP, BVFRQ, ZTMP, SIGF, ALPHAB

C     TTRAVL  = Travel time

C     Variable Initializations
      MODNAM = 'SIGZ'

      IF( STABLE .OR. (UNSTAB .AND. HS .GE. ZI) )THEN
C        The atmosphere is stable or the release was above the CBL mixing ht.
C        See Eq. 1 of the document by Venkatram referenced above.

         TTRAVL = XARG / UEFF

C----    Apply Sigma-Z formulation from CTDMPLUS
C----    Locate index below HE, and retrieve potential temperature at HE

         CALL LOCATE(GRIDHT, 1, MXGLVL, HE, NDXZHE)

         CALL GINTRP( GRIDHT(NDXZHE), GRIDPT(NDXZHE),
     &      GRIDHT(NDXZHE+1), GRIDPT(NDXZHE+1), HE, PTP )

         IF (TGEFF .GT. 0.0D0) THEN
            BVFRQ = DSQRT( G * TGEFF / PTP )
         ELSE
            BVFRQ = 1.0D-10
         END IF

         IF(BVFRQ .LT. 1.0D-10) BVFRQ = 1.0D-10

C        Set height for calculating sigma-z, ZTMP
         ZTMP = MAX( HS, HE, 1.0D-4 )

         IF (URBSTAB) THEN
C           Set BVF term to zero for urban stable boundary layer
            SZAMB = SWEFF * TTRAVL / DSQRT( 1.0D0 + SWEFF*TTRAVL *
     &                        ( 1.0D0/(0.72D0*ZTMP) ) )

         ELSE
            SZAMB = SWEFF * TTRAVL / DSQRT( 1.0D0 + SWEFF*TTRAVL *
     &                 ( 1.0D0/(0.72D0*ZTMP) + BVFRQ/(0.54D0*SWEFF) ) )
         END IF

         IF (HE .LT. ZI) THEN
            CALL SZSFCL (XARG)

            SIGF = MIN ( HE/ZI, 1.0D0)
            SZAS = (1.0D0 - SIGF) * SZSURF + SIGF * SZAMB
         ELSE
            SZAS = SZAMB
         END IF


      ELSEIF( UNSTAB )THEN
C        The atmosphere is unstable and the release is below the mixing ht.
C        See Eqs. 5c and 24a of the document by Weil referenced above
C        SZAD1 = ambient sigma_Z for the direct plume updraft
C        SZAD2 = ambient sigma_Z for the direct plume downdraft
C        SZAN1 = ambient sigma_Z for the indirect plume updraft
C        SZAN2 = ambient sigma_Z for the indirect plume downdraft

         IF (PPF .LT. 1.0D0) THEN
            IF (.NOT.SURFAC) THEN
               ALPHAB = 1.0D0
            ELSE
               ALPHAB = 0.6D0 + 0.4D0*(CENTER/(0.1D0*ZI))
            END IF

            SZAD1 = ALPHAB * BSUB1 * WSTAR * XARG / UEFFD
            SZAD2 = ALPHAB * BSUB2 * WSTAR * XARG / UEFFD

            CALL SZSFCL (XARG)

            SZAD1 = DSQRT( SZAD1*SZAD1 + SZSURF*SZSURF )
            SZAD2 = DSQRT( SZAD2*SZAD2 + SZSURF*SZSURF )

         ELSE
            SZAD1 = 1.0D0
            SZAD2 = 1.0D0

         END IF

         SZAN1 = SZAD1
         SZAN2 = SZAD2

C        Calculate the ambient sigma_Z for a penetrated plume, SZA3
         IF( PPF .GT. 0.0D0 )THEN

            TTRAVL = XARG / UEFF3

C----       Apply Sigma-Z formulation from CTDMPLUS
C----       Set BVF term to zero for penetrated plume
            SZA3 = SWEFF3 * TTRAVL / DSQRT( 1.0D0 + SWEFF3*TTRAVL *
     &                          ( 1.0D0/(0.72D0*HE3) ) )

         ELSE
            SZA3 = 0.0D0
         ENDIF

      ENDIF

      RETURN
      END

      SUBROUTINE BID
C***********************************************************************
C             BID Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Applies Bouyancy-Induced Dispersion to
C                 Sigma-y and Sigma-z
C
C        PROGRAMMER: Roger Brode and Jim Paumier, PES, Inc
C
C        DATE:    September 30, 1993
C
C        INPUTS:  Sigma-y
C                 Sigma-z
C                 Downwind Distance
C                 Buoyancy and Momentum Fluxes
C                 Source Parameter Arrays
C
C        OUTPUTS: BID contributions to sigma_Y and sigma_Z
C
C        CALLED FROM:   PDIS
C
C        References:  "A Dispersion Model for the Convective Boundary
C                      Layer (Revised)", J. Weil, 8/27/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'BID'

C     Calculate the buoyancy-induced contribution, which will be added
C     to the other contributions in RMSSIG

      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN

         SZB = BETA2 * DHP / RTOF2

         SYB = SZB

      ELSEIF( UNSTAB )THEN

C        Direct source contribution
         SZBD = BETA2 * DHP1 / RTOF2

C        Set SZBN = SZBD.
         SZBN = SZBD

C        The penetrated source contribution
         IF( UNSTAB  .AND.  (PPF .GT. 0.0D0) )THEN
            SZB3 = BETA2 * PPF * DHP3 / RTOF2

         ELSE
            SZB3 = 0.0D0

         ENDIF

         SYB  = SZBD
         SYB3 = SZB3

      ENDIF

      RETURN
      END

      SUBROUTINE SZSFCL (XARG)
C***********************************************************************
C             SZSFCL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the surface layer dispersion term for sigma_Z
C
C        PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc.
C
C        DATE:    September 30, 1993
C
C        REVISIONS:  SZSURF formula revised according to P.D.F. Model 
C                    for Dispersion in the Convective Boundary Layer, 
C                    J.C. Weil.  Revised 7/13/94, R.F. Lee.
C
C                    Fixed stable SZSURF to match MFD.  R. Brode, PES, 12/5/94
C
C        INPUTS:  Stack height (HS)
C                 Mixing height (ZI)
C                 Friction velocity (USTAR)
C                 Downwind distance (XARG)
C                 Effective wind speed (UEFF)
C                 Monin-Obukhov length (OBULEN)
C
C        OUTPUTS: Surface layer dispersion contribution to sigma_Z
C
C        CALLED FROM:   PDIS
C
C        References:  "A Dispersion Model for the Convective Boundary
C                      Layer", J. Weil, 8/27/93
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      DOUBLE PRECISION :: XARG

C     Variable Initializations
      MODNAM = 'SZSFCL'

C NOTE --->  BSUBC = 0.5 is set in a PARAMETER stmt in MODULE MAIN1

C---- Calculate the surface layer contribution, which will be added
C     to the other contributions in RMSSIG, from Eqn 31a

      IF (UNSTAB .AND. SURFAC) THEN
         SZSURF = BSUBC * ( 1.0D0 - 10.0D0 * (CENTER / ZI)) *
     &            (USTAR / UEFFD)*(USTAR / UEFFD)  *
     &            (XARG * XARG / DABS( OBULEN ))

      ELSEIF (STABLE) THEN

         SZSURF = (RTOF2/RTOFPI) * USTAR * (XARG/UEFF) *
     &            (1.0D0 + 0.7D0*XARG/OBULEN)**(-THIRD)

      ELSE
         SZSURF = 0.0D0

      ENDIF

      RETURN
      END

      SUBROUTINE RMSSIG
C***********************************************************************
C             RMSSIG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Calculates the root-mean-square value of sigma_Y and
C                 sigma_Z
C
C        PROGRAMMER: Jim Paumier and Roger Brode, PES, Inc.
C
C        REVISIONS:  Stable treatment of surface releases revised 
C                    according to Summary of AERMOD Equations, 
C                    A. Venkatram, 7/7/94.  Revision made 7/13/94, 
C                    R.F. Lee.
C
C        DATE:    September 30, 1993
C
C        INPUTS:  Ambient terms
C                 Virtual source terms
C                 Buoyancy induced terms
C                 Surface layer term
C
C        OUTPUTS: Total sigma_Y (SY) and sigma_Z (SZ)
C
C        CALLED FROM:   PDIS
C
C        References:
C
C
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'RMSSIG'

C---- Calculate the root-mean-square sigma values

      IF( STABLE  .OR.  (UNSTAB .AND. (HS .GE. ZI) ) )THEN
C----    The atmosphere is stable or the atmosphere is unstable and the
C        release is above the mixing height

         SY = DSQRT( SYB*SYB + SYAMB*SYAMB + VSIGY*VSIGY )

         SZ = DSQRT( SZB*SZB + SZAS*SZAS + VSIGZ*VSIGZ )

      ELSEIF( UNSTAB )THEN
C----    The atmosphere is unstable and the release is below the mixing ht.

         SY   = DSQRT( SYB*SYB + SYAMB*SYAMB + VSIGY*VSIGY )
         SYN  = DSQRT( SYB*SYB + SYAN*SYAN + VSYN*VSYN )
         SZD1 = DSQRT( SZBD*SZBD + SZAD1*SZAD1 + VSZD1*VSZD1 )
         SZD2 = DSQRT( SZBD*SZBD + SZAD2*SZAD2 + VSZD2*VSZD2 )
         SZN1 = DSQRT( SZBN*SZBN + SZAN1*SZAN1 + VSZN1*VSZN1 )
         SZN2 = DSQRT( SZBN*SZBN + SZAN2*SZAN2 + VSZN2*VSZN2 )

         IF( PPF .GT. 0.0D0 )THEN
            SY3 = DSQRT( SYB3*SYB3 + SYA3*SYA3 )
            SZ3 = DSQRT( SZB3*SZB3 + SZA3*SZA3 )

         ELSE
            SY3 = 0.0D0
            SZ3 = 0.0D0

         ENDIF

      ENDIF

      RETURN
      END
