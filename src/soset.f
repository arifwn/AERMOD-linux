      SUBROUTINE SOCARD
C***********************************************************************
C                 SOCARD Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: To process SOurce Pathway card images
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        MODIFIED:   Modified ISSTAT() indices to eliminate potential
C                    conflicts among different options.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED BY  D. Strimaitis, SRC (for WET DEPOSITION)
C
C        DATE:    November  8, 1993
C
C        MODIFIED BY  D. Strimaitis, SRC (for DRY DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Pathway (SO) and Keyword
C
C        OUTPUTS: Source Arrays
C                 Sourcer Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, ILSAVE

C     Variable Initializations
      MODNAM = 'SOCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
C        Initialize Counters and Set Status Switch
         ISRC = 0
         IGRP = 0
         NUMSRC = 0
         NUMGRP = 0
         NUMOLM = 0
         NUMPSD = 0
         NUMCAP = 0
         NUMHOR = 0
         NURBSRC = 0
         NUMFLAT = 0
         ISSTAT(1) = ISSTAT(1) + 1
         IF (ISSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF
C        Flush The Working Area
         IWRK2(:,:) = 0
      ELSE IF (KEYWRD .EQ. 'LOCATION') THEN
C        Set Status Switch
         ISSTAT(2) = ISSTAT(2) + 1
C        Check for SRCGROUP or PSDGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
C        Process Source Location                            ---   CALL SOLOCA
         CALL SOLOCA
      ELSE IF (KEYWRD .EQ. 'SRCPARAM') THEN
C        Set Status Switch
         ISSTAT(3) = ISSTAT(3) + 1
C        Check for SRCGROUP or PSDGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
C        Process Source Parameters                          ---   CALL SOPARM
         CALL SOPARM

c --- PRIME ---------------------------------
      ELSE IF (KEYWRD .EQ. 'BUILDHGT' .OR.
     &         KEYWRD .EQ. 'BUILDWID' .OR.
     &         KEYWRD .EQ. 'BUILDLEN' .OR.
     &         KEYWRD .EQ. 'XBADJ   ' .OR.
     &         KEYWRD .EQ. 'YBADJ   ') THEN
c -------------------------------------------

C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
C        Set Status Switch
         IF (KEYWRD .EQ. 'BUILDHGT') THEN
            ISSTAT(4) = ISSTAT(4) + 1
         ELSE IF (KEYWRD .EQ. 'BUILDWID') THEN
            ISSTAT(5) = ISSTAT(5) + 1

c --- PRIME -----------------------------------
         ELSE IF (KEYWRD .EQ. 'BUILDLEN') THEN
            ISSTAT(21) = ISSTAT(21) + 1
         ELSE IF (KEYWRD .EQ. 'XBADJ   ') THEN
            ISSTAT(22) = ISSTAT(22) + 1
         ELSE IF (KEYWRD .EQ. 'YBADJ   ') THEN
            ISSTAT(23) = ISSTAT(23) + 1
c ---------------------------------------------

         END IF
C        Process Direction-specific Building Dimensions     ---   CALL DSBLDG
         CALL DSBLDG
      ELSE IF (KEYWRD .EQ. 'EMISFACT') THEN
C        Set Status Switch
         ISSTAT(7) = ISSTAT(7) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
C        Process Variable Emission Rate Factors             ---   CALL EMVARY
         CALL EMVARY
      ELSE IF (KEYWRD .EQ. 'EMISUNIT') THEN
C        Set Status Switch
         ISSTAT(8) = ISSTAT(8) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (ISSTAT(8) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (NUMTYP .EQ. 1) THEN
C           Process Emission Rate Unit Conversion Factors   ---   CALL EMUNIT
            CALL EMUNIT
         ELSE
C           WRITE Error Message: EMISUNIT Keyword with more than 1 output type
            CALL ERRHDL(PATH,MODNAM,'E','158',' ')
         END IF
      ELSE IF (KEYWRD .EQ. 'PARTDIAM' .OR. KEYWRD .EQ. 'MASSFRAX' .OR.
     &         KEYWRD .EQ. 'PARTDENS') THEN
C        Set Status Switch
         IF (KEYWRD .EQ. 'PARTDIAM') THEN
            ISSTAT(9) = ISSTAT(9) + 1
         ELSE IF (KEYWRD .EQ. 'MASSFRAX') THEN
            ISSTAT(10) = ISSTAT(10) + 1
         ELSE IF (KEYWRD .EQ. 'PARTDENS') THEN
            ISSTAT(11) = ISSTAT(11) + 1
         END IF
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
C        Process Particle Deposition Parameters             ---   CALL PARTDEP
         CALL PARTDEP

      ELSE IF (KEYWRD .EQ. 'ELEVUNIT') THEN
C        Set Status Switch
         ISSTAT(15) = ISSTAT(15) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (ISSTAT(15) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE IF (NUMSRC .GT. 0) THEN
C           Write Error Message: ELEVUNIT must be first card after STARTING
            CALL ERRHDL(PATH,MODNAM,'E','152','  SO')
         ELSE
C           Process Elevation Units for Source Elevations   ---   CALL SOELUN
            CALL SOELUN
         END IF
      ELSE IF (KEYWRD .EQ. 'HOUREMIS') THEN
C*       Set Status Switch
         ISSTAT(16) = ISSTAT(16) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
C        Set HOURLY Flag
         HOURLY = .TRUE.
C*       Process Hourly Emissions                           ---   CALL HREMIS
         CALL HREMIS
C*#

      ELSE IF (KEYWRD .EQ. 'CONCUNIT') THEN
C        Set Status Switch
         ISSTAT(17) = ISSTAT(17) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (ISSTAT(17) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
         IF (ISSTAT(8) .NE. 0) THEN
C           WRITE Error Message: Conflict with EMISUNIT
            CALL ERRHDL(PATH,MODNAM,'E','159',KEYWRD)
         ELSE
C           Process Emission Rate Unit Conversion Factors   ---   CALL COUNIT
            CALL COUNIT
         END IF
      ELSE IF (KEYWRD .EQ. 'DEPOUNIT') THEN
C        Set Status Switch
         ISSTAT(18) = ISSTAT(18) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (ISSTAT(18) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
         IF (ISSTAT(8) .NE. 0) THEN
C           WRITE Error Message: Conflict with EMISUNIT
            CALL ERRHDL(PATH,MODNAM,'E','159',KEYWRD)
         ELSE
C           Process Emission Rate Unit Conversion Factors   ---   CALL DPUNIT
            CALL DPUNIT
         END IF

      ELSE IF (KEYWRD .EQ. 'AREAVERT') THEN
C        Set Status Switch
         ISSTAT(19) = ISSTAT(19) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
C        Process Vertices for AREAPOLY Sources              ---   CALL ARVERT
         CALL ARVERT

      ELSE IF (KEYWRD .EQ. 'INCLUDED') THEN
C        Set Status Switch
         ISSTAT(20) = ISSTAT(20) + 1
C        Save ILINE as ISAVE
         ILSAVE = ILINE
C        Process the Included Receptor File                 ---   CALL INCLUD
         CALL INCLUD
C        Retrieve ILINE From ISAVE
         ILINE = ILSAVE

      ELSE IF (KEYWRD .EQ. 'SRCGROUP') THEN
C        Set Status Switch
         ISSTAT(24) = ISSTAT(24) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT. PSDCREDIT) THEN
C           Process Source Groups                           ---   CALL SOGRP
            CALL SOGRP
         ELSE
C           Write Error Message: SRCGROUP specified with PSDCREDIT option
            CALL ERRHDL(PATH,MODNAM,'E','105',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'GASDEPOS') THEN
C        Set Status Switch
         ISSTAT(26) = ISSTAT(26) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (.NOT. LUSERVD) THEN
C           Process Gas Deposition Parameters               ---   CALL GASDEP
            CALL GASDEP
         ELSE 
C           Write Error Message:  User-specified deposition velocity
            CALL ERRHDL(PATH,MODNAM,'E','195',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'METHOD_2') THEN
C        Set Status Switch
         ISSTAT(27) = ISSTAT(27) + 1
C ---    Check for DFAULT option; METHOD_2 is considered non-DFAULT
         IF (DFAULT) THEN
C           Write Error Message:  METHOD_2 w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','197','METHOD_2')
         ELSE
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
C           Check for SRCGROUP Card Out Of Order
            IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
            ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
            END IF
C           Process Method 2 Deposition Parameters          ---   CALL METH_2
            CALL METH_2
         END IF

      ELSE IF (KEYWRD .EQ. 'URBANSRC') THEN
C        Set Status Switch
         ISSTAT(28) = ISSTAT(28) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (URBAN) THEN
C           Process the Urban Source Card                   ---   CALL URBANS
            CALL URBANS
         ELSE
C           Write Error Message:  Urban source defined without URBANOPT card
            CALL ERRHDL(PATH,MODNAM,'E','130','URBANOPT')
         END IF

      ELSE IF (KEYWRD .EQ. 'NO2RATIO') THEN
C        Set Status Switch
         ISSTAT(29) = ISSTAT(29) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (PVMRM .OR. OLM) THEN
C           Process the NO2 Ratio Card                      ---   CALL NO2RAT
            CALL NO2RAT
         ELSE
C           Write Error Message:  NO2RATIO specified without PVMRM or OLM
            CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'OLMGROUP') THEN
C        Set Status Switch
         ISSTAT(30) = ISSTAT(30) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (OLM) THEN
C           Process the OLM Group Card                      ---   CALL OLMGRP
            CALL OLMGRP
         ELSE
C           Write Error Message:  OLMGROUP specified without OLM
            CALL ERRHDL(PATH,MODNAM,'E','144',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'PSDGROUP') THEN 
C        Set Status Switch
         ISSTAT(34) = ISSTAT(34) + 1
C        Check for PSDGROUP Card Out Of Order
         IF (PSDCREDIT) THEN
C           Process the PSD Group Card                      ---   CALL PSDGRP
            CALL PSDGRP
         ELSE
C           Write Error Message: PSDGROUP specified without PSDCREDIT option
            CALL ERRHDL(PATH,MODNAM,'E','146',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'BACKGRND') THEN 
C        Set Status Switch
         ISSTAT(40) = ISSTAT(40) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
C        Process the BACKGRND Card                         ---   CALL BACK_GRND
         CALL BACK_GRND

      ELSE IF (KEYWRD .EQ. 'BACKUNIT') THEN 
C        Set Status Switch
         ISSTAT(41) = ISSTAT(41) + 1
C        Check for SRCGROUP Card Out Of Order
         IF (.NOT.PSDCREDIT .AND. ISSTAT(24) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         ELSE IF (PSDCREDIT .AND. ISSTAT(34) .NE. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','PSDGROUP')
         END IF
         IF (ISSTAT(41) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process the BACKUNIT Card                      ---   CALL BACK_UNIT
            CALL BACK_UNIT
         END IF

      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         ISSTAT(50) = ISSTAT(50) + 1
         IF (ISSTAT(50) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF

C        Check to Insure That SRCGROUP or PSDGROUP Was The Last Functional Keyword
         IF (PKEYWD .NE. 'SRCGROUP' .AND. .NOT.PSDCREDIT) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF
         IF (PKEYWD .NE. 'PSDGROUP' .AND. PSDCREDIT) THEN
            CALL ERRHDL(PATH,MODNAM,'E','140','SRCGROUP')
         END IF

C        Check for Missing Mandatory Keywords
         IF (ISSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ISSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','LOCATION')
         END IF
         IF (ISSTAT(3) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCPARAM')
         END IF
         IF (ISSTAT(24) .EQ. 0 .AND. .NOT.PSDCREDIT) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCGROUP')
         END IF
         IF (ISSTAT(34) .EQ. 0 .AND. PSDCREDIT) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','PSDGROUP')
         END IF
         IF (ISSTAT(3) .LT. ISSTAT(2)) THEN
C           Must Be Missing a SRCPARAM Card for One or More Sources
            CALL ERRHDL(PATH,MODNAM,'E','130','SRCPARAM')
         END IF

C ---    Check for BACKUNIT keyword without BACKGRND keyword
         IF (ISSTAT(40) .EQ. 0 .AND. ISSTAT(41) .GT. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','193','SO BACKGRND')
         END IF
         
C ---    Check for number of source = 0
         IF (NUMSRC .EQ. 0) THEN
C           WRITE Error Message:  No Sources Input
            CALL ERRHDL(PATH,MODNAM,'E','248','NUMSRC=0')
            
         ELSE
C ---       Check for non-DFAULT gas deposition options
            IF (DFAULT .AND. ISSTAT(26) .GT. 0) THEN
C              Write Error Message:  Gas Deposition Option w/ DFAULT Option
               CALL ERRHDL(PATH,MODNAM,'E','196','GASDEPOS')
            ELSE IF (ISSTAT(26) .GT. 0) THEN
C              Set flag for use of non-DEFAULT option
               L_NonDFAULT = .TRUE.
            END IF
C ---       Set logical flags for deposition options
            IF (LUSERVD .OR. (ICSTAT(18).GT.0 .AND. 
     &                        ICSTAT(20).GT.0 .AND.
     &                        ISSTAT(26).GT.0)) THEN
C ---          Gas dry deposition inputs specified
C              GASDEPVD or GDSEASON, GDLANUSE & GASDEPOS
               LDGAS = .TRUE.
            END IF
            IF (ISSTAT(26) .GT. 0) THEN
C ---          Gas wet deposition inputs specified
               LWGAS = .TRUE.
            END IF
            IF ((ISSTAT( 9).GT.0 .AND. ISSTAT(10).GT.0 .AND.
     &           ISSTAT(11).GT.0) .OR. ISSTAT(27).GT.0) THEN
C ---          Particle dry and wet deposition inputs specified
C              PARTDIAM, MASSFRAX & PARTDENS or METHOD_2
               LDPART = .TRUE.
               LWPART = .TRUE.
            END IF
            IF (DRYDPLT .OR. (.NOT.NODRYDPLT .AND. 
     &                       (LDGAS .OR. LDPART))) THEN
C ---          Set dry depletion unless overridden by user
               DDPLETE = .TRUE.
            END IF
            IF (WETDPLT .OR. (.NOT.NOWETDPLT .AND. 
     &                       (LWGAS .OR. LWPART))) THEN
C ---          Set wet depletion unless overridden by user
               WDPLETE = .TRUE.
            END IF

C ---       Check for incompatibilities with user-specified deposition velocity
C           This condition may not be caught by check in subroutine COCARD
            IF (LUSERVD .AND. WDPLETE) THEN
C              Write Error Message: Wet deposition/depletion incompatible 
C              with GASDEPVD option
               CALL ERRHDL(PATH,MODNAM,'E','243','GASDEPVD')
            END IF

C ---       Set model option header for dry depletion
            IF (DDPLETE) THEN
               IF (ARDPLETE) THEN
                  MODOPS(14) = 'AREADPLT'
               ELSE IF (ROMBERG) THEN
                  MODOPS(14) = 'ROMBERG'
               ELSE
                  MODOPS(14) = 'DRYDPLT'
               END IF
            ELSE IF (NODRYDPLT) THEN
               MODOPS(14) = 'NODRYDPLT'
            ELSE
               MODOPS(14) = '         '
            END IF
C ---       Set model option header for wet depletion
            IF (WDPLETE) THEN
               MODOPS(15) = 'WETDPLT'
            ELSE IF (NODRYDPLT) THEN
               MODOPS(15) = 'NOWETDPLT'
            ELSE
               MODOPS(15) = '         '
            END IF
C ---       Check for error with inputs for dry deposition
            IF ((DDPLETE .OR. DEPOS .OR. DDEP) .AND. 
     &          (.NOT.LDGAS .AND. .NOT.LDPART)) THEN
               CALL ERRHDL('SO',MODNAM,'E','244','DRYDEP')
            END IF
C ---       Check for error with inputs for wet deposition
            IF ((WDPLETE .OR. DEPOS .OR. WDEP) .AND. 
     &          (.NOT.LWGAS .AND. .NOT.LWPART)) THEN
               CALL ERRHDL('SO',MODNAM,'E','244','WETDEP')
            END IF
            
C ---       Quality Assure Source Parameter Inputs          ---   CALL SRCQA
            CALL SRCQA
            
C ---       Check for consistency of deposition logical variables
C           Check for CO GDSEASON Card if Gas Deposition is Calculated
            IF (.NOT. LUSERVD .AND. LDGAS .AND. ICSTAT(18) .EQ. 0) THEN
C ---          Write Error Message:  Missing Mandatory Keyword
               CALL ERRHDL('CO',MODNAM,'E','130','GDSEASON')
            END IF
C ---       Check for CO GDLANUSE Card if Gas Deposition is Calculated
            IF (.NOT. LUSERVD .AND. LDGAS .AND. ICSTAT(20) .EQ. 0) THEN
C              Write Error Message:  Missing Mandatory Keyword
               CALL ERRHDL('CO',MODNAM,'E','130','GDLANUSE')
            END IF
C ---       Check for SO GASDEPOS Card if Gas Depos is Calculated w/o LUSERVD
            IF (.NOT. LUSERVD .AND. LDGAS .AND. ISSTAT(26) .EQ. 0) THEN
C              Write Error Message:  Missing Mandatory Keyword
               CALL ERRHDL('SO',MODNAM,'E','130','GASDEPOS')
            END IF

C ---       Calculate settling velocity and related time-invariant
C           deposition data                                 ---   CALL VDP1
            IF (LDPART .OR. LDGAS) THEN
               CALL VDP1
            END IF

C ---       Reassign MODOPS(1) character for use of non-DFAULT options
C           MODOPS(1) initially assigned in sub. COCARD, but source inputs
C           may have changed value of L_NonDFAULT.
            IF (DFAULT) THEN
               MODOPS(1) = 'RegDFAULT'
            ELSE IF (L_NonDFAULT) THEN
               MODOPS(1) = 'NonDFAULT'
            ELSE
               MODOPS(1) = '         '
            END IF
            
         END IF

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE SRCQA
C***********************************************************************
C                 SRCQA Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Quality Assure Source Parameter Inputs
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C        MODIFIED BY D. Strimaitis, SRC (for WET & DRY DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED:   Modified checks for urban area without urban
C                    sources, and changed warning to fatal error
C                    for urban areas with no urban sources.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Include check for source being defined as
C                    both particulate and gaseous emissions.
C                    To include options to vary emissions by
C                    hour-of-day, and day-of-week (HRDOW and HRDOW7).
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Modified calculation of area and center coordinates
C                    for AREAPOLY sources to use DOUBLE PRECISION. This
C                    change avoids problems encountered with the
C                    Compaq Visual Fortran compiler producing erroneous
C                    results for some compiler options.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/24/2007
C
C        MODIFIED:   Calculates equivalent XINIT and YINIT values for
C                    AREAPOLY sources to allow for calculation of area
C                    of source under TOXICS option.  Also includes a
C                    a more refined computation of centroid for
C                    AREAPOLY sources.
C                    R.W. Brode, MACTEC (f/k/a PES), Inc., 7/23/2004
C
C        MODIFIED:   To include an option to vary emissions by season,
C                    hour-of-day, and day-of-week (SHRDOW).
C                    R.W. Brode, PES, 4/10/2000
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Source Parameters
C                 Source Parameters Array Limits, IWRK2(NSRC,13)
C
C        OUTPUTS: Source Parameter Error Messages
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      LOGICAL :: FOPEN
      INTEGER :: I, J, N, ITOTSRC, ITOTGRP, NumBack
      INTEGER :: ILSAVE
      DOUBLE PRECISION :: ATOT
      DOUBLE PRECISION :: SUMA, SUMX, SUMY, AREA

C     Variable Initializations
      MODNAM = 'SRCQA'
      FOPEN  = .FALSE.

C     Begin Source LOOP
      DO I = 1, NUMSRC

C        Check Source Array Limits for Too Few Values;
C        (Too Many Checked In DSFILL and EFFILL)
         IF (IWRK2(I,1) .GT.0 .OR. IWRK2(I,2) .GT.0 .OR.
     &       IWRK2(I,3) .GT.0 .OR. IWRK2(I,11).GT.0 .OR.
     &       IWRK2(I,12).GT.0 .OR. IWRK2(I,13).GT.0) THEN

            IF (IWRK2(I,1).LT.NSEC) THEN
C              WRITE Error Message:  Not Enough BUILDHGTs
               CALL ERRHDL(PATH,MODNAM,'E','236',SRCID(I))
            END IF
            IF (IWRK2(I,2).LT.NSEC) THEN
C              WRITE Error Message:  Not Enough BUILDWIDs
               CALL ERRHDL(PATH,MODNAM,'E','237',SRCID(I))
            END IF

c --- PRIME -------------------------------------------------
            IF (IWRK2(I,11).LT.NSEC) THEN
C              WRITE Error Message:  Not Enough BUILDLENs
               CALL ERRHDL(PATH,MODNAM,'E','241',SRCID(I))
            END IF
            IF (IWRK2(I,12).LT.NSEC) THEN
C              WRITE Error Message:  Not Enough XBADJs
               CALL ERRHDL(PATH,MODNAM,'E','246',SRCID(I))
            END IF
            IF (IWRK2(I,13).LT.NSEC) THEN
C              WRITE Error Message:  Not Enough YBADJs
               CALL ERRHDL(PATH,MODNAM,'E','247',SRCID(I))
            END IF
c -----------------------------------------------------------
         END IF

         IF (QFLAG(I) .NE. ' ') THEN
            IF (QFLAG(I).EQ.'SEASON' .AND. IWRK2(I,4).LT.4) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF (QFLAG(I).EQ.'MONTH' .AND. IWRK2(I,4).LT.12) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'HROFDY' .AND. IWRK2(I,4).LT.24) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF (QFLAG(I).EQ.'WSPEED' .AND. IWRK2(I,4).LT.6) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'SEASHR' .AND. IWRK2(I,4).LT.96) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'HRDOW' .AND. IWRK2(I,4).LT.72) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'HRDOW7' .AND. IWRK2(I,4).LT.168) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'SHRDOW' .AND. IWRK2(I,4).LT.288) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'SHRDOW7' .AND. IWRK2(I,4).LT.672) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'MHRDOW' .AND. IWRK2(I,4).LT.864) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I).EQ.'MHRDOW7' .AND. IWRK2(I,4).LT.2016) THEN
C              WRITE Error Message: Not Enough QFACTs
               CALL ERRHDL(PATH,MODNAM,'E','239',SRCID(I))
            ELSE IF(QFLAG(I) .EQ. 'HOURLY') THEN
C              Check for use of hourly-varying sigmas and release heights 
C              for VOLUME and AREA source types;
C              Call HRQREAD subroutine for each source to set L_HRLYSIG flag;
C              First save ILINE and reset to 1 to trigger L_HRLYSIG check
               ILSAVE = ILINE
               IQLINE = IQLINE + 1
               ILINE  = 1
               KURDAT = 0
               CALL HRQREAD(I)
               ILINE  = ILSAVE
            END IF
         END IF

C        Check Settling and Removal Parameters
         IF (IWRK2(I,5).NE.0 .OR. IWRK2(I,6).NE.0 .OR.
     &       IWRK2(I,7).NE.0) THEN
C           Set Number of Particle Diameter Categories for This Source
            INPD(I) = IWRK2(I,5)
C           Check for Consistent Number of Categories for All Parameters
            IF (IWRK2(I,5).NE.IWRK2(I,6) .OR.
     &          IWRK2(I,5).NE.IWRK2(I,7)) THEN
C              WRITE Error Message: PartDiam Categories Don't Match
               CALL ERRHDL(PATH,MODNAM,'E','240',SRCID(I))
            END IF
C           Check for Mass Fraction Summing to 1.0 (+/- 2%)
            ATOT = 0.0D0
            N = INPD(I)
            IF (N .LE. NPDMAX) THEN
               DO J = 1, N
                  ATOT = ATOT + APHI(J,I)
               END DO
               IF (ATOT .LT. 0.98D0 .OR. ATOT .GT. 1.02D0) THEN
C                 WRITE Error Message: Mass Fractions Don't Sum to 1.0
                  CALL ERRHDL(PATH,MODNAM,'W','330',SRCID(I))
               END IF
            ELSE
C              WRITE Error Message:  Too Many Settling/Removal Categories
               WRITE(DUMMY,'(I8)') NPDMAX
               CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
            END IF
         END IF

C        Screen for Conflicts with the Deposition Options
         IF (INPD(I) .EQ. 0) THEN
C           Check for NPD=0 and no gas deposition with the DEPOS, DDEP, or WDEP
            IF ((DEPOS.OR.DDEP.OR.WDEP.OR.DDPLETE.OR.WDPLETE) .AND. 
     &           .NOT. LUSERVD .AND. SOGAS(I).EQ.'N') THEN
C              WRITE Error Message for Lack of Gas or Particle Deposition Parameters
               CALL ERRHDL(PATH,MODNAM,'E','242',SRCID(I))
            END IF
         ELSE IF (INPD(I).GT.0 .AND. SOGAS(I).EQ.'Y') THEN
C           Check for NPD>0 and gas deposition for same source
C           WRITE Error Message for source as both particle and gas
            CALL ERRHDL(PATH,MODNAM,'E','289',SRCID(I))
         END IF

C        Check Vertices and Determine Centroid for AREAPOLY Sources
         IF (SRCTYP(I) .EQ. 'AREAPOLY') THEN
            IF (IWRK2(I,10) .LT. NVERTS(I)) THEN
C              WRITE Error Message:  Not Enough Vertices Input For This Source
               CALL ERRHDL(PATH,MODNAM,'E','265',SRCID(I))
            ELSE
C              Repeat First Vertex as Last Vertex to Close Polygon
               AXVERT(NVERTS(I)+1,I) = AXVERT(1,I)
               AYVERT(NVERTS(I)+1,I) = AYVERT(1,I)

C              Determine coordinates for centroid of polygon source;
C              First calculate area of polygon
               suma = 0.0D0
               do j = 1, NVERTS(I)
                  suma = suma + (AXVERT(j,i)  *AYVERT(j+1,i) -
     &                           AXVERT(j+1,i)*AYVERT(j,i))
               end do

               area = 0.5D0 * suma

C              Assign SQRT(DABS(area)) to AXINIT and AYINIT; equivalent values
C              of AXINIT and AYINIT will be used to calculate area of polygon
               AXINIT(I) = DSQRT( DABS(area) )
               AYINIT(I) = DSQRT( DABS(area) )

C              Now determine coordinates of centroid
               sumx = 0.0D0
               sumy = 0.0D0
               do j = 1, NVERTS(I)
                  sumx = sumx +(AXVERT(j,i)+AXVERT(j+1,i)) *
     &                         (AXVERT(j,i)*AYVERT(j+1,i) -
     &                          AXVERT(j+1,i)*AYVERT(j,i))
                  sumy = sumy +(AYVERT(j,i)+AYVERT(j+1,i)) *
     &                         (AXVERT(j,i)*AYVERT(j+1,i) -
     &                          AXVERT(j+1,i)*AYVERT(j,i))
               end do

               IF (DABS(area) .lt. 0.0001D0) THEN
C                 WRITE Error Message:  Invalid shape for AREAPOLY source
                  CALL ERRHDL(PATH,MODNAM,'E','266',SRCID(I))
                  AXCNTR(I) = 0.0D0
                  AYCNTR(I) = 0.0D0
               ELSE
C                 Calculate coordinates of centroid
                  AXCNTR(I) = sumx/(6.0D0*area)
                  AYCNTR(I) = sumy/(6.0D0*area)
               END IF

            END IF
         END IF

C        Check for urban sources
         IF (URBSRC(I) .EQ. 'Y') THEN
            NURBSRC = NURBSRC + 1
         END IF

C        Check for capped and horizontal stack sources
         IF (SRCTYP(I) .EQ. 'POINTCAP') THEN
            NUMCAP = NUMCAP + 1
         ELSE IF (SRCTYP(I) .EQ. 'POINTHOR') THEN
            NUMHOR = NUMHOR + 1
         END IF

C        Identify the index of the level immediately below the top of the
C        stack from the array of gridded heights; we are limiting the
C        number of levels to search to 29 (= 600 m).  (Changed from 21
C        by R. Brode, PES, 2/17/95)

         CALL LOCATE( GRIDHT, 1, 29, AHS(I), NDXSTK(I) )

      END DO
C     End Source LOOP

C     Check for open HOUREMIS file; if so, rewind file
      IF (HOURLY) THEN
C        Check for Hourly Emissions File
         INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
         IF (FOPEN) THEN
            REWIND IHREMI
            IQLINE = 0
         END IF
      END IF

      IF (PSDCREDIT) THEN
CRWB     Assign default "source groups" for PSDCREDIT applications:
CRWB     first "source group" contains cumulative NAAQS calculation;
CRWB     second "source group" contains PSD increment consumtion with credits.
         NUMGRP = 2
         GRPID(1) = 'NAAQS   '
         GRPID(2) = 'PSDINC  '

C        Check for source not included in any PSDGROUP
         DO I = 1, NUMSRC
            ITOTSRC = 0
            DO J = 1, NUMPSD
               IF (IGRP_PSD(I,J) .EQ. 1) THEN
                  ITOTSRC = ITOTSRC + 1
               END IF
            END DO
            IF (ITOTSRC .EQ. 0) THEN
C              Write Error Message:  Source not in PSDGROUP
               CALL ERRHDL(PATH,MODNAM,'E','317',SRCID(I))
            END IF
         END DO                     

      ELSE

C        Check for empty source groups and count how many groups
C        include BACKGROUND
         NumBack = 0
         DO J = 1, NUMGRP
            ITOTSRC = 0
            DO I = 1, NUMSRC
               IF (IGROUP(I,J) .EQ. 1) THEN
                  ITOTSRC = ITOTSRC + 1
               END IF
            END DO
            IF (GRP_BACK(J)) THEN
               NumBack = Numback + 1
            END IF
            IF (ITOTSRC .EQ. 0 .AND. .NOT.GRP_BACK(J)) THEN
C              Write Warning Message:  No Sources in SRCGROUP
               CALL ERRHDL(PATH,MODNAM,'W','319',GRPID(J))
            END IF
         END DO

C ---    Issue warning if BACKGROUND not included with any
C        source groups
         IF (L_BACKGRND .AND. NumBack .EQ. 0) THEN
C           Write Warning Message:  BACKGROUND not in SRCGROUP
            CALL ERRHDL(PATH,MODNAM,'W','317','BACKGROUND')
         END IF
         
C        Check for source not included in any source group
         DO I = 1, NUMSRC
            ITOTSRC = 0
            DO J = 1, NUMGRP
               IF (IGROUP(I,J) .EQ. 1) THEN
                  ITOTSRC = ITOTSRC + 1
               END IF
            END DO
            IF (ITOTSRC .EQ. 0) THEN
C              Write Warning Message:  Source not in SRCGROUP
               CALL ERRHDL(PATH,MODNAM,'W','317',SRCID(I))
            END IF
         END DO                     

      END IF

      IF (URBAN .AND. NURBSRC.EQ.0) THEN
C        Write Error Message:  No urban sources defined with URBANOPT
         CALL ERRHDL(PATH,MODNAM,'E','130','URBANSRC')
      END IF

C     Check for Urban Areas with No Sources;
C     (single urban area checked based on missing URBANSRC card)
      IF (NUMURB .GT. 1) THEN
         DO J = 1, NUMURB
            ITOTSRC = 0
            DO I = 1, NUMSRC
               IF (IURBGRP(I,J) .EQ. 1) THEN
                  ITOTSRC = ITOTSRC + 1
               END IF
            END DO
            IF (ITOTSRC .EQ. 0) THEN
C              Write Error Message:  No Sources for Urban Area
               CALL ERRHDL(PATH,MODNAM,'E','318',URBID(J))
            END IF
         END DO
      END IF

C     Check for source in more than one Urban Area
      DO I = 1, NUMSRC
         ITOTGRP = 0
         DO J = 1, NUMURB
            IF (IURBGRP(I,J) .EQ. 1) THEN
               ITOTGRP = ITOTGRP + 1
            END IF
         END DO
         IF (ITOTGRP .GT. 1) THEN
C           Write Error Message:  Source in more than one Urban Area
            CALL ERRHDL(PATH,MODNAM,'E','302',SRCID(I))
         END IF
      END DO

C     Check for source in more than one OLMGROUP
      DO I = 1, NUMSRC
         ITOTGRP = 0
         DO J = 1, NUMOLM
            IF (IGRP_OLM(I,J) .EQ. 1) THEN
               ITOTGRP = ITOTGRP + 1
            END IF
         END DO
         IF (ITOTGRP .GT. 1) THEN
C           Write Error Message:  Source in more than one OLMGROUP
            CALL ERRHDL(PATH,MODNAM,'E','282',SRCID(I))
         END IF
      END DO

C     Check for source in more than one PSDGROUP                        ! jop 9/30/06
      IF (PSDCREDIT) THEN
         DO I = 1, NUMSRC
            ITOTGRP = 0
            DO J = 1, NUMPSD
               IF (IGRP_PSD(I,J) .EQ. 1) THEN
                  ITOTGRP = ITOTGRP + 1
               END IF
            END DO
            IF (ITOTGRP .GT. 1) THEN
C              Write Error Message:  Source in more than one PSDGROUP
               CALL ERRHDL(PATH,MODNAM,'E','286',SRCID(I))
            END IF
         END DO
      END IF

      IF (OLM .OR. PVMRM) THEN
C        Check for negative emission rate with OLM or PVMRM option.
C        Negative emission for credit sources cannot be used for OLM and
C        PVMRM due to non-linear dependence of concentrations on emissions.
CRWB     Draft BETA-test option for PSDCREDIT accounts for non-linear effects
CRWB     on PSD increment calculations for PVMRM, but does not use negative
CRWB     emission rates to identify PSD credit sources.
         DO I = 1, NUMSRC
            IF (AQS(I) .LT. 0.0D0) THEN
C              Write Error Message:  Negative emission rate for OLM or PVMRM
               CALL ERRHDL(PATH,MODNAM,'E','338',SRCID(I))
            END IF
         END DO

C        Check for negative in-stack NO2/NOx ratio with OLM or PVMRM option.
C        This indicates that user has not specified an in-stack ratio for
C        a particular source.  This is an error since no default ratio has
C        been determined for use with OLM or PVMRM with the 1-hour NO2 NAAQS.
         DO I = 1, NUMSRC
            IF (ANO2_RATIO(I) .LT. 0.0D0) THEN
C              Write Error Message:  No in-stack ratio specified for PVMRM
               CALL ERRHDL(PATH,MODNAM,'E','336',SRCID(I))
            END IF
         END DO
      END IF
      
      IF (L_BACKGRND) THEN
C ---    Check for correct number of temporally varying background concentrations
C        for the BACKGRND keyword.
         IF (IBKGRD .LT. NBF) THEN
C           WRITE Error Message: Not Enough BACKGRND values
            WRITE(DUMMY,'(''NBF ='',I6)') NBF
            CALL ERRHDL(PATH,MODNAM,'E','238',DUMMY)
         END IF
C ---    Check for user-specified background units; apply default if needed
         IF (ISSTAT(41) .NE. 1) THEN
            IF (POLLUT .EQ. 'NO2' .OR. POLLUT .EQ. 'SO2') THEN
               BackUnits = 'PPB'
            ELSE IF (POLLUT .EQ. 'CO') THEN
               BackUnits = 'PPM'
            ELSE
               BackUnits = 'UG/M3'
            END IF
         END IF
      END IF

      RETURN
      END

      SUBROUTINE SOELUN
C***********************************************************************
C                 SOELUN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Elevation Units Option for Sources
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    November 22, 1994
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Elevation Units Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'SOELUN'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'METERS') THEN
            SOELEV = 'METERS'
         ELSE IF (FIELD(3) .EQ. 'FEET') THEN
            SOELEV = 'FEET'
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203','SO_ELEV')
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200','ElevUnit')
      END IF

 999  RETURN
      END

      SUBROUTINE SOLOCA
C***********************************************************************
C                 SOLOCA Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source Location Card
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C*       MODIFIED BY: Jayant Hardikar (PES) 7/19/94 to incorporate
C*                    new "PIT" source type.
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Type and Location
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: INDEXS
      CHARACTER (LEN=12) :: SOID
      LOGICAL FOUND

C     Variable Initializations
      FOUND  = .FALSE.
      MODNAM = 'SOLOCA'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 6) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Read In The Data Fields and Assign to Arrays
C     Check for Previous Occurrence of This SRCID
C*    First check for length of SRCID field (<=12)
      IF ((LOCE(3)-LOCB(3)) .LE. 11) THEN
C*       Retrieve Source ID Character Substring
         SOID = FIELD(3)
C ---    Check for "reserved" source ID for 'BACKGROUND'
         IF (SOID .EQ. 'BACKGROUND') THEN
C*          WRITE Error Message:  Source ID Field is Too Long
            CALL ERRHDL(PATH,MODNAM,'E','230',FIELD(3)(1:12))
            GO TO 999
         END IF
      ELSE
C*       WRITE Error Message:  Source ID Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','230',FIELD(3)(1:12))
         GO TO 999
      END IF

      CALL SINDEX(SRCID,NSRC,SOID,INDEXS,FOUND)

      IF (.NOT. FOUND) THEN
         ISRC = ISRC + 1
         IF (ISRC .LE. NSRC) THEN
            SRCID(ISRC)  = FIELD(3)
            SRCTYP(ISRC) = FIELD(4)

C ---       Allow for variations in the source type string
            IF (SRCTYP(ISRC) .EQ. 'OPENPIT'  .OR.
     &          SRCTYP(ISRC) .EQ. 'OPEN_PIT' .OR.
     &          SRCTYP(ISRC) .EQ. 'OPEN-PIT') THEN
               SRCTYP(ISRC) = 'OPENPIT'
            END IF

            IF ((SRCTYP(ISRC).EQ.'POINTCAP' .OR.
     &           SRCTYP(ISRC).EQ.'POINTHOR') .AND.
     &           .NOT.BETA) THEN
C              WRITE Error Message: BETA Option Req'd for POINTCAP/POINTHOR
               CALL ERRHDL(PATH,MODNAM,'E','199',SRCTYP(ISRC))
            END IF

            IF (SRCTYP(ISRC).EQ.'POINT' .OR.
     &          SRCTYP(ISRC).EQ.'POINTCAP' .OR.
     &          SRCTYP(ISRC).EQ.'POINTHOR' .OR.
     &          SRCTYP(ISRC).EQ.'VOLUME' .OR.
     &          SRCTYP(ISRC).EQ.'AREA' .OR.
     &          SRCTYP(ISRC).EQ.'AREAPOLY' .OR.
     &          SRCTYP(ISRC).EQ.'AREACIRC' .OR.
     &          SRCTYP(ISRC).EQ.'OPENPIT') THEN

               CALL STODBL(FIELD(5), ILEN_FLD, AXS(ISRC), IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               CALL STODBL(FIELD(6), ILEN_FLD, AYS(ISRC), IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
               IF (IFC .EQ. 7 .AND. FLATSRCS .AND. 
     &                              FIELD(7) .EQ. 'FLAT') THEN
C ---              Source has been identified as a terrain-following
C                  source, to be simulated with 'FLAT' terrain option
                   L_FLATSRC(ISRC) = .TRUE.
                   NUMFLAT = NUMFLAT + 1
               ELSE IF (IFC .EQ. 7) THEN
C                 Retrieve Source Elevation From Inputs
                  CALL STODBL(FIELD(7), ILEN_FLD, AZS(ISRC), IMIT)
C                 Check The Numerical Field
                  IF (IMIT .NE. 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  END IF
C ---             Check for missing source elevations, coded as -9999.0,
C                 and convert from FEET to METERS if needed
                  IF (AZS(ISRC) .LT. -9998.99D0) THEN
C                    WRITE Error Message:  Source elevation is missing
                     CALL ERRHDL(PATH,MODNAM,'E','249',SOID)
                  ELSE IF (SOELEV .EQ. 'FEET') THEN
                     AZS(ISRC) = AZS(ISRC) * 0.3048D0
                  END IF
               ELSE
C                 No Source Elevation Field - Default to 0.0
                  AZS(ISRC) = 0.0D0
                  IF (ELEV) THEN
C                    Write Warning Message for No Source Elevation with ELEV
                     CALL ERRHDL(PATH,MODNAM,'W','205','ZS = 0.0')
                  END IF
               END IF
            ELSE
C              Error Message: Invalid Source Type
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCTYP')
               GO TO 999
            END IF
            ISET = ISRC
            NUMSRC = NUMSRC + 1
         ELSE
C           WRITE Error Message    ! Number of Sources Exceeds NSRC Parameter
            WRITE(DUMMY,'(I8)') NSRC
            CALL ERRHDL(PATH,MODNAM,'E','232',DUMMY)
            GO TO 999
         END IF
      ELSE
C        WRITE Error Message    ! Source Location Has Already Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','310',SOID)
      END IF

 999  RETURN
      END

      SUBROUTINE SOPARM
C***********************************************************************
C                 SOPARM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source parameter Card
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for additional parameters on area source
C                    parameter cards for new algorithm - 7/7/93
C
C*       MODIFIED BY: Jayant Hardikar (PES) 7/19/94 to incorporate
C*                    new "PIT" source type.
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameters
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ISDX
      LOGICAL FOUND
      DOUBLE PRECISION :: TEMP(IFMAX)

C     Variable Initializations
      FOUND  = .FALSE.
      MODNAM = 'SOPARM'
      TEMP   = 0.0D0

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Search For The Source ID Index
      CALL SINDEX(SRCID,NSRC,FIELD(3),ISDX,FOUND)

      IF (FOUND) THEN
C        Check for Previous SRCPARAM Card for This Source
         IF (SOPCRD(ISDX) .EQ. 'Y') THEN
C           WRITE Error Message: Duplicate SRCPARAM Card
            CALL ERRHDL(PATH,MODNAM,'E','315',SRCID(ISDX))
            GO TO 999
         ELSE
            SOPCRD(ISDX) = 'Y'
         END IF
C        Assign The Parameter Arrays
         DO I = 4, IFC
            CALL STODBL(FIELD(I),ILEN_FLD,TEMP(I-3),IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
         END DO
         IF (SRCTYP(ISDX) .EQ. 'POINT' .OR.
     &       SRCTYP(ISDX) .EQ. 'POINTCAP' .OR.
     &       SRCTYP(ISDX) .EQ. 'POINTHOR') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 8) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 8) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL PPARM(ISDX,TEMP)
         ELSE IF (SRCTYP(ISDX) .EQ. 'VOLUME') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 7) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 7) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL VPARM(ISDX,TEMP)
         ELSE IF (SRCTYP(ISDX) .EQ. 'AREA') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 6) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 9) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL APARM(ISDX,TEMP)
         ELSE IF (SRCTYP(ISDX) .EQ. 'AREAPOLY') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 6) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 7) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL APPARM(ISDX,TEMP)
         ELSE IF (SRCTYP(ISDX) .EQ. 'AREACIRC') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 6) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 8) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL ACPARM(ISDX,TEMP)
C*       Get Source Parameters for the OPENPIT source
         ELSE IF (SRCTYP(ISDX) .EQ. 'OPENPIT') THEN
            IF (IFC .EQ. 3) THEN
C              Error Message: No Parameters
               CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
               GO TO 999
            ELSE IF (IFC .LT. 8) THEN
C              Error Message: Not Enough Parameters
               CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
               GO TO 999
            ELSE IF (IFC .GT. 9) THEN
C              Error Message: Too Many Parameters
               CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
               GO TO 999
            END IF
            CALL OPARM(ISDX,TEMP)

         END IF
      ELSE
C        WRITE Error Message    ! Source Location Has Not Been Identified Yet
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE PPARM(ISDX,TEMP)
C***********************************************************************
C                 PPARM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source Parameters for POINT Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To assign default factor to adjust initial diameter
C                    of plume for capped stacks for use in the PRIME
C                    algorithm, for BETA-test draft option.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ISDX
      DOUBLE PRECISION TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'PPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      ATS(ISDX) = TEMP(3)
      AVS(ISDX) = TEMP(4)
      ADS(ISDX) = TEMP(5)

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0D0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 600.0D0) THEN
C        WRITE Warning Message:  Large Release Height (> 600M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      ELSE IF (AHS(ISDX) .GT. 3000.0D0) THEN
C        WRITE Error Message:  Large Release Height (> 3000M)
         CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
      END IF

      IF (ATS(ISDX) .EQ. 0.0D0) THEN
C        Set Temperature to Small Negative Value for Ambient Releases
         ATS(ISDX) = -1.0D-5
      ELSE IF (ATS(ISDX) .GT. 2000.0D0) THEN
C        WRITE Warning Message:  Exit Temp. > 2000K
         CALL ERRHDL(PATH,MODNAM,'W','320',' TS ')
      END IF

      IF (AVS(ISDX) .LT. 0.0D0) THEN
C        WRITE Warning Message:  Negative or Zero Exit Velocity
         CALL ERRHDL(PATH,MODNAM,'W','325',SRCID(ISDX))
C        Set to Small Value to Avoid Zero-divide and Underflow
         AVS(ISDX) = 1.0D-5
      ELSE IF (AVS(ISDX) .LT. 1.0D-5) THEN
C        Set to Small Value to Avoid Zero-divide and Underflow
         AVS(ISDX) = 1.0D-5
      ELSE IF (AVS(ISDX) .GT. 50.0D0) THEN
C        WRITE Warning Message:  Exit Velocity > 50.0 m/s
         CALL ERRHDL(PATH,MODNAM,'W','320',' VS ')
      END IF

      IF (ADS(ISDX) .LT. 0.0D0) THEN
C        WRITE Warning Message:  Negative Stack Diameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' DS ')
      ELSE IF (ADS(ISDX) .LT. 1.0D-5) THEN
C        Set to Small Value to Avoid Zero-divide and Underflow
         ADS(ISDX) = 1.0D-5
      ELSE IF (ADS(ISDX) .GT. 20.0D0) THEN
C        WRITE Warning Message:  Large Stack Diameter (> 20m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' DS ')
      END IF

C     Assign default factor to adjust stack diameter for capped stack
C     releases at 2.0 (i.e. cap doubles initial diameter of plume) for
C     use in the PRIME algorithm for BETA-test draft option.
      IF (SRCTYP(ISDX) .EQ. 'POINTCAP') THEN
         ADSFACT(ISDX) = 2.0D0
      ELSE
         ADSFACT(ISDX) = 1.0D0
      END IF

      RETURN
      END

      SUBROUTINE VPARM(ISDX,TEMP)
C***********************************************************************
C                 VPARM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source Parameters for VOLUME Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ISDX
      DOUBLE PRECISION TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'VPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      ASYINI(ISDX) = TEMP(3)
      ASZINI(ISDX) = TEMP(4)

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0D0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 100.0D0) THEN
C        WRITE Warning Message:  Large Release Height (> 100M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      ELSE IF (AHS(ISDX) .GT. 3000.0D0) THEN
C        WRITE Error Message:  Large Release Height (> 3000M)
         CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
      END IF

      IF (ASYINI(ISDX) .LT. 0.0D0) THEN
C        WRITE Warning Message:  Negative Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SYINIT ')
      ELSE IF (ASYINI(ISDX) .LT. 1.0D-5) THEN
C        WRITE Warning Message:  Small Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'W','320',' SYINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         ASYINI(ISDX) = 1.0D-5
      ELSE IF (ASYINI(ISDX) .GT. 200.0D0) THEN
C        WRITE Warning Message:  Large Initial Lateral Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SYINIT ')
      END IF

      IF (ASZINI(ISDX) .LT. 0.0D0) THEN
C        WRITE Warning Message:  Negative Initial Vertical Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
      ELSE IF (ASZINI(ISDX) .LT. 1.0D-5) THEN
C        WRITE Warning Message:  Small Initial Lateral Parameter
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         ASZINI(ISDX) = 1.0D-5
      ELSE IF (ASZINI(ISDX) .GT. 200.0D0) THEN
C        WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
      END IF

      RETURN
      END

      SUBROUTINE APARM(ISDX,TEMP)
C***********************************************************************
C                 APARM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source Parameters for AREA Sources
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To allow for additional parameters on area source
C                    parameter cards for new algorithm - 7/7/93
C
C        MODIFIED:   Corrected IF-BLOCK for error checking - 7/21/94
C
C        MODIFIED BY Roger Brode, PES (modified data structure for
C                    AXVERT and AYVERT for consistency with other
C                    2-D source arrays) - 8/15/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ISDX
      DOUBLE PRECISION TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'APARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      IF (IFC .EQ. 6) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = AXINIT(ISDX)
         AANGLE(ISDX) = 0.0D0
         ASZINI(ISDX) = 0.0D0
      ELSE IF (IFC .EQ. 7) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = 0.0D0
         ASZINI(ISDX) = 0.0D0
      ELSE IF (IFC .EQ. 8) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = TEMP(5)
         ASZINI(ISDX) = 0.0D0
      ELSE IF (IFC .EQ. 9) THEN
         AXINIT(ISDX) = TEMP(3)
         AYINIT(ISDX) = TEMP(4)
         AANGLE(ISDX) = TEMP(5)
         ASZINI(ISDX) = TEMP(6)
      END IF

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0D0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 100.0D0) THEN
C        WRITE Warning Message:  Large Release Height (> 100M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      ELSE IF (AHS(ISDX) .GT. 3000.0D0) THEN
C        WRITE Error Message:  Large Release Height (> 3000M)
         CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
      END IF

      IF (AXINIT(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' XINIT ')
      ELSE IF (AXINIT(ISDX) .LT. 1.0D-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AXINIT(ISDX) = 1.0D-5
      ELSE IF (AXINIT(ISDX) .GT. 2000.0D0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
      END IF
      
      IF (AYINIT(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' YINIT ')
      ELSE IF (AYINIT(ISDX) .LT. 1.0D-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AYINIT(ISDX) = 1.0D-5
      ELSE IF (AYINIT(ISDX) .GT. 2000.0D0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
      END IF

      IF (DABS(AANGLE(ISDX)) .GT. 180.0D0 ) THEN
C        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
         CALL ERRHDL(PATH,MODNAM,'W','320',' ANGLE ')
      END IF

      IF (ASZINI(ISDX) .LT. 0.0D0) THEN
C*       WRITE Warning Message:  Negative Initial Vertical Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
      ELSE IF (ASZINI(ISDX) .LT. 1.0D-5) THEN
C*       Set to Small Value to Avoid Zero-divide and Underflow
         ASZINI(ISDX) = 1.0D-5
      ELSE IF (ASZINI(ISDX) .GT. 200.0D0) THEN
C*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
      END IF

C     Check for aspect ratio (length/width) > 100
      IF (AYINIT(ISDX)/AXINIT(ISDX) .GT. 100.00001D0 .OR.
     &    AXINIT(ISDX)/AYINIT(ISDX) .GT. 100.00001D0) THEN
C        WRITE Warning Message: Aspect ratio > 100 for area source
         CALL ERRHDL(PATH,MODNAM,'W','391',SRCID(ISDX))
      END IF

C     Set Number of Vertices (4 for Rectangular Source)
      NVERTS(ISDX) = 4

C     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
C     Vertices Start with the "Southwest" Corner and Are Defined
C     Clockwise.  The First Vertex is Repeated as the Last Vertex.

      AXVERT(1,ISDX) = AXS(ISDX)
      AYVERT(1,ISDX) = AYS(ISDX)

      AXVERT(2,ISDX) = AXVERT(1,ISDX) +
     &                (AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
      AYVERT(2,ISDX) = AYVERT(1,ISDX) +
     &                (AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

      AXVERT(3,ISDX) = AXVERT(2,ISDX) +
     &                (AXINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))
      AYVERT(3,ISDX) = AYVERT(2,ISDX) -
     &                (AXINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))

      AXVERT(4,ISDX) = AXVERT(3,ISDX) -
     &                (AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
      AYVERT(4,ISDX) = AYVERT(3,ISDX) -
     &                (AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

      AXVERT(5,ISDX) = AXS(ISDX)
      AYVERT(5,ISDX) = AYS(ISDX)

C     Determine coordinates for center of rectangular source
      AXCNTR(ISDX) = 0.0D0
      AYCNTR(ISDX) = 0.0D0
      DO I = 1, NVERTS(ISDX)
         AXCNTR(ISDX) = AXCNTR(ISDX) + AXVERT(I,ISDX)
         AYCNTR(ISDX) = AYCNTR(ISDX) + AYVERT(I,ISDX)
      END DO
      AXCNTR(ISDX) = AXCNTR(ISDX)/DBLE(NVERTS(ISDX))
      AYCNTR(ISDX) = AYCNTR(ISDX)/DBLE(NVERTS(ISDX))

      RETURN
      END

      SUBROUTINE APPARM(ISDX,TEMP)
C***********************************************************************
C                 APPARM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source Parameters for AREAPOLY Sources
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    August 14, 1995
C
C        MODIFIED:   Removed NINT(int_variable).  R. Brode, PES, 11/21/97
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ISDX
      DOUBLE PRECISION TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'APPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      NVERTS(ISDX) = IDNINT(TEMP(3))
      IF (IFC .EQ. 7) THEN
         ASZINI(ISDX) = TEMP(4)
      ELSE
         ASZINI(ISDX) = 0.0D0
      END IF

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0D0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 100.0D0) THEN
C        WRITE Warning Message:  Large Release Height (> 100M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      ELSE IF (AHS(ISDX) .GT. 3000.0D0) THEN
C        WRITE Error Message:  Large Release Height (> 3000M)
         CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
      END IF

      IF (ASZINI(ISDX) .LT. 0.0D0) THEN
C*       WRITE Warning Message:  Negative Initial Vertical Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
      ELSE IF (ASZINI(ISDX) .LT. 1.0D-5) THEN
C*       Set to Small Value to Avoid Zero-divide and Underflow
         ASZINI(ISDX) = 1.0D-5
      ELSE IF (ASZINI(ISDX) .GT. 200.0D0) THEN
C*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
      END IF

      IF (NVERTS(ISDX) .LT. 3) THEN
C        WRITE Error Message:  Not Enough Vertices
         CALL ERRHDL(PATH,MODNAM,'E','380',' NVERT ')
      ELSE IF (NVERTS(ISDX) .GT. NVMAX-8) THEN
C        WRITE Error Message:  Too Many Vertices
         CALL ERRHDL(PATH,MODNAM,'E','406',SRCID(ISDX))
      END IF

      RETURN
      END

      SUBROUTINE ARVERT
C***********************************************************************
C                 ARVERT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Vertices for AREAPOLY Sources
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:    August 15, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Area Sources Vertices
C
C        CALLED FROM:   SOCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: K, ISDX
      DOUBLE PRECISION :: DNUMX, DNUMY
      CHARACTER (LEN=12) :: SOID
      LOGICAL FOUND

C     Variable Initializations
      FOUND  = .FALSE.
      MODNAM = 'ARVERT'

C     Get The Source ID, and
C     Check for Previous Occurrence of This SRCID
C*    First check for length of SRCID field (<=12)
      IF ((LOCE(3)-LOCB(3)) .LE. 11) THEN
C*       Retrieve Source ID Character Substring
         SOID = FIELD(3)
C ---    Check for "reserved" source ID for 'BACKGROUND'
         IF (SOID .EQ. 'BACKGROUND') THEN
C*          WRITE Error Message:  Source ID Field is Too Long
            CALL ERRHDL(PATH,MODNAM,'E','230',FIELD(3)(1:12))
            GO TO 999
         END IF
      ELSE
C*       WRITE Error Message:  Source ID Field is Too Long
         CALL ERRHDL(PATH,MODNAM,'E','230',FIELD(3)(1:12))
         GO TO 999
      END IF

C     Search For The Index
      CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
      IF (FOUND) THEN
         ISET = IWRK2(ISDX,10)
         DO K = 4, IFC-1, 2
C           Change Fields To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUMX,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
            CALL STODBL(FIELD(K+1),ILEN_FLD,DNUMY,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF

            ISET = ISET + 1
            IF (ISET .EQ. 1) THEN
C              Compare First Vertex to Source Location
               IF (DNUMX .NE. AXS(ISDX)) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','262',SRCID(ISDX))
               END IF
               IF (DNUMY .NE. AYS(ISDX)) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','262',SRCID(ISDX))
               END IF
            END IF

            IF (ISET .LE. NVERTS(ISDX)) THEN
C              Assign The Field
               AXVERT(ISET,ISDX) = DNUMX
               AYVERT(ISET,ISDX) = DNUMY
            ELSE
C              WRITE Error Message: Too Many Vertices For This Source
               CALL ERRHDL(PATH,MODNAM,'E','264',SRCID(ISDX))
            END IF
         END DO
         IWRK2(ISDX,10) = ISET
      ELSE
C        WRITE Error Message     ! Source Location Has Not Been Identified
         CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE ACPARM(ISDX,TEMP)
C***********************************************************************
C                 ACPARM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source Parameters for AREACIRC Sources
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 15, 1995
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: ISDX
      DOUBLE PRECISION :: TEMP(IFMAX)

C     Variable Initializations
      MODNAM = 'ACPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      RADIUS(ISDX) = TEMP(3)
      IF (IFC .GE. 7) THEN
C ---    Assign NVERTS for this source
         NVERTS(ISDX) = IDNINT(TEMP(4))
      ELSE
         NVERTS(ISDX) = 20
      END IF
      IF (IFC .EQ. 8) THEN
         ASZINI(ISDX) = TEMP(5)
      ELSE
         ASZINI(ISDX) = 0.0D0
      END IF

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0D0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 100.0D0) THEN
C        WRITE Warning Message:  Large Release Height (> 100M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      ELSE IF (AHS(ISDX) .GT. 3000.0D0) THEN
C        WRITE Error Message:  Large Release Height (> 3000M)
         CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
      END IF

      IF (RADIUS(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Radius
         CALL ERRHDL(PATH,MODNAM,'E','209',' RADIUS ')
      ELSE IF (RADIUS(ISDX) .LE. 0.5D0) THEN
C        WRITE Error Message:  Invalid Value for Radius
         CALL ERRHDL(PATH,MODNAM,'E','203',' RADIUS ')      
      ELSE IF (RADIUS(ISDX) .GT. 10000.0D0) THEN
C        WRITE Warning Message:  Large Radius (> 10000M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' RADIUS ')
      END IF

      IF (ASZINI(ISDX) .LT. 0.0D0) THEN
C*       WRITE Warning Message:  Negative Initial Vertical Parameter
         CALL ERRHDL(PATH,MODNAM,'E','209',' SZINIT ')
      ELSE IF (ASZINI(ISDX) .LT. 1.0D-5) THEN
C*       Set to Small Value to Avoid Zero-divide and Underflow
         ASZINI(ISDX) = 1.0D-5
      ELSE IF (ASZINI(ISDX) .GT. 200.0D0) THEN
C*       WRITE Warning Message:  Large Initial Vertical Parameter (> 200m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' SZINIT ')
      END IF

      IF (NVERTS(ISDX) .LT. 3) THEN
C        WRITE Error Message:  Not Enough Vertices
         CALL ERRHDL(PATH,MODNAM,'E','380',' NVERT ')
         GO TO 999
      ELSE IF (NVERTS(ISDX) .GT. NVMAX-4) THEN
C        WRITE Error Message:  Too Many Vertices
         CALL ERRHDL(PATH,MODNAM,'E','406',SRCID(ISDX))
         GO TO 999
      END IF

C     Setup Vertices for Circular Area
      CALL GENCIR(ISDX)

C     Set coordinates for center of circular source
      AXCNTR(ISDX) = AXS(ISDX)
      AYCNTR(ISDX) = AYS(ISDX)

 999  RETURN
      END

      SUBROUTINE GENCIR(ISDX)
C***********************************************************************
C                 GENCIR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Generates Vertices for Circular Area Source
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:    September 15, 1995
C
C        MODIFIED:   Corrected variable type from INTEGER to REAL
C                    for NEWRAD.  R.W. Brode, EPA, 11/1/06
C
C        INPUTS:  Center of circle
C                 Radius of circle
C                 Number of vertices
C
C        OUTPUTS: Arrays of vertices
C
C        CALLED FROM:   ACPARM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ISDX, NSIDES
      DOUBLE PRECISION :: ANG, ANGINC, AREA, TRIAREA, OPP, NEWRAD

C     Variable Initializations
      MODNAM = 'GENCIR'

      NSIDES = NVERTS(ISDX)
      ANGINC = 360.0D0/DBLE(NSIDES)
      ANG = 0.0D0

C     Calculate New Radius That Will Provide An Equal-Area Polygon
      AREA = PI * RADIUS(ISDX) * RADIUS(ISDX)
      TRIAREA = AREA/DBLE(NSIDES)
      OPP = DSQRT(TRIAREA * DTAN(ANGINC/(2.0D0*RTODEG)) )
      NEWRAD = OPP / (DSIN(ANGINC/(2.0D0*RTODEG)) )

C     Generate Vertices for Circular Area of NSIDES
      DO I = 1, NSIDES
         IF (I .NE. 1) ANG = ANG+ANGINC

         AXVERT(I,ISDX) = (NEWRAD * DSIN(ANG/RTODEG)) + AXS(ISDX)
         AYVERT(I,ISDX) = (NEWRAD * DCOS(ANG/RTODEG)) + AYS(ISDX)
      END DO

C     Repeat First Vertex as Last Vertex to Close the Area
      AXVERT(NSIDES+1,ISDX) = AXVERT(1,ISDX)
      AYVERT(NSIDES+1,ISDX) = AYVERT(1,ISDX)

C     Assign SQRT(area) to AXINIT and AYINIT; equivalent values
C     of AXINIT and AYINIT will be used to calculate area of source
      AXINIT(ISDX) = DSQRT( area )
      AYINIT(ISDX) = DSQRT( area )

      RETURN
      END

      SUBROUTINE OPARM(ISDX,TEMP)
C***********************************************************************
C                 OPARM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source Parameters for OPENPIT Sources
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C                    (based on APARM - Jeff Wang/Roger Brode)
C
C        DATE:       July 19, 1994
C
C        MODIFIED BY Roger Brode, PES (modified data structure for
C                    AXVERT and AYVERT for consistency with other
C                    2-D source arrays) - 8/15/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Parameter Card
C
C        CALLED FROM:   SOPARM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ISDX
      DOUBLE PRECISION TEMP(IFMAX), EFFDEP

C     Variable Initializations
      MODNAM = 'OPARM'

      AQS(ISDX) = TEMP(1)
      AHS(ISDX) = TEMP(2)
      AXINIT(ISDX) = TEMP(3)
      AYINIT(ISDX) = TEMP(4)
      AVOLUM(ISDX) = TEMP(5)
      AANGLE(ISDX) = 0.0D0
      IF (IFC .EQ. 9) THEN
         AANGLE(ISDX) = TEMP(6)
      END IF

C     Perform QA Error Checking on Source Parameters

      IF (AQS(ISDX) .EQ. 0.0D0) THEN
C        WRITE Warning Message:  Emission Rate Equals 0.0
         CALL ERRHDL(PATH,MODNAM,'W','320',' QS ')
      END IF

      IF (AHS(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Release Height
         CALL ERRHDL(PATH,MODNAM,'E','209',' HS ')
      ELSE IF (AHS(ISDX) .GT. 200.0D0) THEN
C        WRITE Warning Message:  Large Release Height (> 200M)
         CALL ERRHDL(PATH,MODNAM,'W','320',' HS ')
      ELSE IF (AHS(ISDX) .GT. 3000.0D0) THEN
C        WRITE Error Message:  Large Release Height (> 3000M)
         CALL ERRHDL(PATH,MODNAM,'E','324',SRCID(ISDX))
      END IF

      IF (AXINIT(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' XINIT ')
      ELSE IF (AXINIT(ISDX) .LT. 1.0D-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AXINIT(ISDX) = 1.0D-5
      ELSE IF (AXINIT(ISDX) .GT. 2000.0D0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' XINIT ')
      END IF
      
      IF (AYINIT(ISDX) .LT. 0.0D0) THEN
C        WRITE Error Message:  Negative Area Size
         CALL ERRHDL(PATH,MODNAM,'E','209',' YINIT ')
      ELSE IF (AYINIT(ISDX) .LT. 1.0D-5) THEN
C        WRITE Warning Message:  Small Source Area
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
C        Set to Small Value to Avoid Zero-divide and Underflow
         AYINIT(ISDX) = 1.0D-5
      ELSE IF (AYINIT(ISDX) .GT. 2000.0D0) THEN
C        WRITE Warning Message:  Large Source Area (> 2000m)
         CALL ERRHDL(PATH,MODNAM,'W','320',' YINIT ')
      END IF

      IF (DABS(AANGLE(ISDX)) .GT. 180.0D0 ) THEN
C        WRITE Warning Message:  Rotation Angle Larger Than 180 Degrees
         CALL ERRHDL(PATH,MODNAM,'W','320',' ANGLE ')
      END IF
      
      IF (AVOLUM(ISDX) .LE. 0.0D0) THEN
C        WRITE Error Message: Open-Pit Volume is less than
C        or equal to zero
         CALL ERRHDL(PATH,MODNAM,'E','209',' AVOLUM ')
      END IF

C     Check for aspect ratio (length/width) > 10
      IF (AYINIT(ISDX)/AXINIT(ISDX) .GT. 10.0D0 .OR.
     &    AXINIT(ISDX)/AYINIT(ISDX) .GT. 10.0D0) THEN
C        WRITE Warning Message: Aspect ratio > 10 for pit source
         CALL ERRHDL(PATH,MODNAM,'W','392',SRCID(ISDX))
      END IF

C     Check for Release Height > Effective Depth
      EFFDEP = AVOLUM(ISDX)/(AXINIT(ISDX)*AYINIT(ISDX))
      IF (AHS(ISDX) .GT. EFFDEP) THEN
C        WRITE Error Message: Release Height is greater than Effective Depth
         CALL ERRHDL(PATH,MODNAM,'E','322',SRCID(ISDX))
      END IF

C     Set Number of Vertices (4 for Rectangular Source)
      NVERT = 4

C     Set Coordinates of Vertices for Rectangular Area (in Kilometers).
C     Vertices Start with the "Southwest" Corner and Are Defined
C     Clockwise.  The First Vertex is Repeated as the Last Vertex.

      AXVERT(1,ISDX) = AXS(ISDX)
      AYVERT(1,ISDX) = AYS(ISDX)

      AXVERT(2,ISDX) = AXVERT(1,ISDX) +
     &                (AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
      AYVERT(2,ISDX) = AYVERT(1,ISDX) +
     &                (AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

      AXVERT(3,ISDX) = AXVERT(2,ISDX) +
     &                (AXINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))
      AYVERT(3,ISDX) = AYVERT(2,ISDX) -
     &                (AXINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))

      AXVERT(4,ISDX) = AXVERT(3,ISDX) -
     &                (AYINIT(ISDX)*DSIN(AANGLE(ISDX)*DTORAD))
      AYVERT(4,ISDX) = AYVERT(3,ISDX) -
     &                (AYINIT(ISDX)*DCOS(AANGLE(ISDX)*DTORAD))

      AXVERT(5,ISDX) = AXS(ISDX)
      AYVERT(5,ISDX) = AYS(ISDX)

C*    Determine the angle of long pit dimension with North
      IF (AYINIT(ISDX) .GE. AXINIT(ISDX)) THEN
         AALPHA(ISDX) = AANGLE(ISDX)
      ELSE IF (AXINIT(ISDX) .GT. AYINIT(ISDX)) THEN
         AALPHA(ISDX) = AANGLE(ISDX) + 90.0D0
      END IF

C*    Calculate the effective pit depth
      APDEFF(ISDX) = AVOLUM(ISDX) / (AXINIT(ISDX) * AYINIT(ISDX))

C*    Calculate Initial Sigma-Z
      ASZINI(ISDX) = APDEFF(ISDX) / 4.3D0

C     Determine coordinates for center of rectangular source
      AXCNTR(ISDX) = 0.0D0
      AYCNTR(ISDX) = 0.0D0
      DO I = 1, NVERT
         AXCNTR(ISDX) = AXCNTR(ISDX) + AXVERT(I,ISDX)
         AYCNTR(ISDX) = AYCNTR(ISDX) + AYVERT(I,ISDX)
      END DO
      AXCNTR(ISDX) = AXCNTR(ISDX)/DBLE(NVERT)
      AYCNTR(ISDX) = AYCNTR(ISDX)/DBLE(NVERT)

      RETURN
      END


      SUBROUTINE DSBLDG
C***********************************************************************
C                 DSBLDG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Direction-specific Building Directions
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IH, IL, ISDX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      FOUND  = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'DSBLDG'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

C     Verify The Effective Srcid
      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (FOUND) THEN
            IF (SRCTYP(ISDX)(1:5) .EQ. 'POINT') THEN
C              Fill Array
               CALL DSFILL(ISDX)
            ELSE
C              WRITE Warning Message: Building Inputs for Non-POINT Source
               CALL ERRHDL(PATH,MODNAM,'W','233',SRCID(ISDX))
            END IF
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP .AND. SRCTYP(I)(1:5).EQ.'POINT') THEN
               ISDX = I
C              Fill DS Array
               CALL DSFILL(ISDX)
            END IF
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE DSFILL(ISDX)
C***********************************************************************
C                 DSFILL Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Fill Direction-specific Building Dimension Arrays
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   DSBLDG
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J, K, ISDX

C     Variable Initializations
      MODNAM = 'DSFILL'

      IF (KEYWRD .EQ. 'BUILDHGT') THEN
         ISET = IWRK2(ISDX,1)
         DO K = 4, IFC
C           Change Fields To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
            DO J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSBH(ISET,ISDX) = DNUM
                  IF (DNUM .LT. 0.0D0) THEN
C                    WRITE Error Message:  Negative Value for ADSBH
                     CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
            END DO
         END DO
         IWRK2(ISDX,1) = ISET
      ELSE IF (KEYWRD .EQ. 'BUILDWID') THEN
         ISET = IWRK2(ISDX,2)
         DO K = 4, IFC
C           Change Fields To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
            DO J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSBW(ISET,ISDX) = DNUM
                  IF (DNUM .LT. 0.0D0) THEN
C                    WRITE Error Message:  Negative Value for ADSBW
                     CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
            END DO
         END DO
         IWRK2(ISDX,2) = ISET

c --- PRIME --------------------------------------------
c --- Fill building length information
      ELSE IF (KEYWRD .EQ. 'BUILDLEN') THEN
         ISET = IWRK2(ISDX,11)
         DO K = 4, IFC
C           Change Fields To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
            DO J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSBL(ISET,ISDX) = DNUM
                  IF (DNUM .LT. 0.0D0) THEN
C                    WRITE Error Message:  Negative value for ADSBL
                     CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
                  END IF
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
            END DO
         END DO
         IWRK2(ISDX,11) = ISET

c --- Fill building XBADJ information
      ELSE IF (KEYWRD .EQ. 'XBADJ   ') THEN
         ISET = IWRK2(ISDX,12)
         DO K = 4, IFC
C           Change Fields To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
            DO J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSXADJ(ISET,ISDX) = DNUM
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
            END DO
         END DO
         IWRK2(ISDX,12) = ISET

c --- Fill building YBADJ information
      ELSE IF (KEYWRD .EQ. 'YBADJ   ') THEN
         ISET = IWRK2(ISDX,13)
         DO K = 4, IFC
C           Change Fields To Numbers
            CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .EQ. -1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               CYCLE
            END IF
            DO J = 1, IMIT
               ISET = ISET + 1
C              Assign The Field
               IF (ISET .LE. NSEC) THEN
                  ADSYADJ(ISET,ISDX) = DNUM
               ELSE
C                 WRITE Error Message    ! Too Many Sectors Input
                  CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
               END IF
            END DO
         END DO
         IWRK2(ISDX,13) = ISET
c --------------------------------------------------------

      END IF

 999  RETURN
      END

      SUBROUTINE EMVARY
C***********************************************************************
C                 EMVARY Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Variable Emission Rate Factors
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To include options to vary emissions by
C                    hour-of-day, and day-of-week (HRDOW and HRDOW7).
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To include options to vary emissions by month,
C                    hour-of-day, and day-of-week (MHRDOW and MHRDOW7).
C                    R.W. Brode, MACTEC (f/k/a PES), Inc., 06/22/05
C
C        MODIFIED:   To replace 'STAR' option with 'WSPEED'.
C                    R.W. Brode, PES, 02/25/02
C
C        MODIFIED:   To include an option to vary emissions by season,
C                    hour-of-day, and day-of-week (SHRDOW).
C                    R.W. Brode, PES, 4/10/2000
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IH, IL, ISDX, IQMAX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      FOUND  = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'EMVARY'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Numerical Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

C     Verify The Effective Srcid
      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (FOUND) THEN
            QFLAG(ISDX) = FIELD(4)
            IF (QFLAG(ISDX) .EQ. 'SEASON') THEN
               IQMAX = 4
            ELSE IF (QFLAG(ISDX) .EQ. 'MONTH') THEN
               IQMAX = 12
            ELSE IF (QFLAG(ISDX) .EQ. 'HROFDY') THEN
               IQMAX = 24
            ELSE IF (QFLAG(ISDX) .EQ. 'WSPEED') THEN
               IQMAX = 6
            ELSE IF (QFLAG(ISDX) .EQ. 'SEASHR') THEN
               IQMAX = 96
            ELSE IF (QFLAG(ISDX) .EQ. 'HRDOW') THEN
               IQMAX = 72
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) .EQ. 'HRDOW7') THEN
               IQMAX = 168
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) .EQ. 'SHRDOW') THEN
               IQMAX = 288
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) .EQ. 'SHRDOW7') THEN
               IQMAX = 672
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) .EQ. 'MHRDOW') THEN
               IQMAX = 864
               L_DayOfWeekOpts = .TRUE.
            ELSE IF (QFLAG(ISDX) .EQ. 'MHRDOW7') THEN
               IQMAX = 2016
               L_DayOfWeekOpts = .TRUE.
            ELSE
C              WRITE Error Message    ! Invalid QFLAG Field Entered
               CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
            END IF
            IF (IQMAX .LE. NQF) THEN
               CALL EFFILL(ISDX,IQMAX)
            ELSE
C              WRITE Error Message     ! NQF Parameter Not Large Enough
               WRITE(DUMMY,'(''NQF ='',I6)') NQF
               CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
            END IF
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               ISDX = I
               QFLAG(ISDX) = FIELD(4)
               IF (QFLAG(ISDX) .EQ. 'SEASON') THEN
                  IQMAX = 4
               ELSE IF (QFLAG(ISDX) .EQ. 'MONTH') THEN
                  IQMAX = 12
               ELSE IF (QFLAG(ISDX) .EQ. 'HROFDY') THEN
                  IQMAX = 24
               ELSE IF (QFLAG(ISDX) .EQ. 'WSPEED') THEN
                  IQMAX = 6
               ELSE IF (QFLAG(ISDX) .EQ. 'SEASHR') THEN
                  IQMAX = 96
               ELSE IF (QFLAG(ISDX) .EQ. 'HRDOW') THEN
                  IQMAX = 72
                  L_DayOfWeekOpts = .TRUE.
               ELSE IF (QFLAG(ISDX) .EQ. 'HRDOW7') THEN
                  IQMAX = 168
                  L_DayOfWeekOpts = .TRUE.
               ELSE IF (QFLAG(ISDX) .EQ. 'SHRDOW') THEN
                  IQMAX = 288
                  L_DayOfWeekOpts = .TRUE.
               ELSE IF (QFLAG(ISDX) .EQ. 'SHRDOW7') THEN
                  IQMAX = 672
                  L_DayOfWeekOpts = .TRUE.
               ELSE IF (QFLAG(ISDX) .EQ. 'MHRDOW') THEN
                  IQMAX = 864
                  L_DayOfWeekOpts = .TRUE.
               ELSE IF (QFLAG(ISDX) .EQ. 'MHRDOW7') THEN
                  IQMAX = 2016
                  L_DayOfWeekOpts = .TRUE.
               ELSE
C                 WRITE Error Message    ! Invalid QFLAG Field Entered
                  CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
               END IF
               IF (IQMAX .LE. NQF) THEN
                  CALL EFFILL(ISDX,IQMAX)
               ELSE
C                 WRITE Error Message    ! NQF Parameter Not Large Enough
                  WRITE(DUMMY,'(''NQF ='',I6)') NQF
                  CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
               END IF
            END IF
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE EFFILL(ISDX,IQMAX)
C***********************************************************************
C                 EFFILL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Fill Variable Emission Rate Array
C
C        PROGRAMMER:  Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   EMVARY
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J, K, ISDX, IQMAX

C     Variable Initializations
      MODNAM = 'EFFILL'

      ISET = IWRK2(ISDX,4)

      DO K = 5, IFC
C        Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. IQMAX) THEN
               QFACT(ISET,ISDX) = DNUM
               IF (DNUM .LT. 0.0D0) THEN
C                 WRITE Error Message:  Negative Value for QFACT
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many QFACT Values Input
               CALL ERRHDL(PATH,MODNAM,'E','231','QFACT')
            END IF
         END DO
      END DO

      IWRK2(ISDX,4) = ISET

      RETURN
      END

      SUBROUTINE EMUNIT
C***********************************************************************
C                 EMUNIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EMUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      EMIFAC(1) = DNUM
      EMILBL(1) = FIELD(4)
      OUTLBL(1) = FIELD(5)
      IF (.NOT.CONC .AND. ANNUAL) THEN
         PERLBL(1) = RUNST1(LOCB(5):LOCE(5))//'/YR'
      ELSE
         PERLBL(1) = FIELD(5)
      END IF

 999  RETURN
      END

      SUBROUTINE COUNIT
C***********************************************************************
C                 COUNIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C                 for CONCentration Values
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'COUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      EMIFAC(1) = DNUM
      EMILBL(1) = FIELD(4)
      OUTLBL(1) = FIELD(5)
      PERLBL(1) = FIELD(5)

 999  RETURN
      END

      SUBROUTINE DPUNIT
C***********************************************************************
C                 DPUNIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Emission Rate Unit Conversion Factors
C                 for Deposition Values
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Emission Rate Unit Conversion Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I

C     Variable Initializations
      MODNAM = 'DPUNIT'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Fetch Each Field
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      IF (.NOT. CONC) THEN
         DO I = 1, NTYP
            EMIFAC(I) = DNUM
            EMILBL(I) = FIELD(4)
            OUTLBL(I) = FIELD(5)
            IF (ANNUAL) THEN
               PERLBL(I) = RUNST1(LOCB(5):LOCE(5))//'/YR'
            ELSE
               PERLBL(I) = FIELD(5)
            END IF
         END DO
      ELSE
         DO I = 2, NTYP
            EMIFAC(I) = DNUM
            EMILBL(I) = FIELD(4)
            OUTLBL(I) = FIELD(5)
            IF (ANNUAL) THEN
               PERLBL(I) = RUNST1(LOCB(5):LOCE(5))//'/YR'
            ELSE
               PERLBL(I) = FIELD(5)
            END IF
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE PARTDEP
C***********************************************************************
C                 PARTDEP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        ADAPTED from  DRYDEP Module of the AMS/EPA Regulatory Model - AERMOD
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        PURPOSE: Processes Inputs for Wet & Dry PARTicle DEPosition
C
C        DRYDEP ADAPTED BY: D. Strimaitis, SRC (for Wet & Dry Deposition)
C        DATE:    November 8, 1993
C
C        DRYDEP MODIFIED BY: D. Strimaitis, SRC (for Dry Deposition)
C        (DATE:    February 15, 1993)
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Input For Setting and Removal Variables
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'PARTDEP'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Numerical Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     Process The Appropriate Settling & Removal Parameter
      IF (KEYWRD .EQ. 'PARTDIAM') THEN
C        Process Particle Diameter Categories (PDIAM)       ---   CALL INPPDM
         CALL INPPDM
      ELSE IF (KEYWRD .EQ. 'MASSFRAX') THEN
C        Process Mass Fractions (PHI)                       ---   CALL INPPHI
         CALL INPPHI
      ELSE IF (KEYWRD .EQ. 'PARTDENS') THEN
C        Process Particle Density (PDENS)                   ---   CALL INPPDN
         CALL INPPDN
      END IF

 999  RETURN
      END

      SUBROUTINE INPPDM
C***********************************************************************
C                 INPPDM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Particle Diameter Categories
C
C        PROGRAMMER: D. Strimaitis, SRC
C
C        ADAPTED FROM "INPVSN"
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Particle Diameter Categories
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, IH, IL, ISDX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      FOUND  = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPPDM'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (FOUND .AND. .NOT.L_METHOD2(ISDX)) THEN
            ISET = IWRK2(ISDX,5)
            DO K = 4, IFC
C              Change It To Numbers
               CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  CYCLE
               ELSE IF (IMIT .GT. 1) THEN
C ---             Use of '*' for repeat input values is not meaningful
C                 for particle diameters; Issue Error message
                  CALL ERRHDL(PATH,MODNAM,'E','288',KEYWRD)
                  CYCLE
               ELSE IF (DNUM .LE. 0.001D0 .OR. DNUM .GT. 1000.0D0) THEN
C                 WRITE Error Message: Particle Diameter Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','335',SRCID(ISDX))
               END IF
               ISET = ISET + 1
               IF (ISET .LE. NPDMAX) THEN
C                 Assign The Field
                  APDIAM(ISET,ISDX) = DNUM
               ELSE
C                 WRITE Error Message: Too Many PartDiam Categories
                  WRITE(DUMMY,'(I8)') NPDMAX
                  CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
               END IF
            END DO
            IWRK2(ISDX,5) = ISET
            
         ELSE IF (FOUND .AND. L_METHOD2(ISDX)) THEN
C ---       Write Error Message     ! Source ID identified as Method 2
            CALL ERRHDL(PATH,MODNAM,'E','386',SRCID(ISDX))

         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         SOURCE_LOOP: DO I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP .AND. .NOT.L_METHOD2(I)) THEN
               ISET = IWRK2(I,5)
               DO K = 4, IFC
C                 Get Numbers
                  CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     CYCLE SOURCE_LOOP
                  ELSE IF (IMIT .GT. 1) THEN
C ---                Use of '*' for repeat input values is not meaningful
C                    for particle diameters; Issue Error message
                     CALL ERRHDL(PATH,MODNAM,'E','288',KEYWRD)
                     CYCLE SOURCE_LOOP
                  ELSE IF (DNUM .LE. 0.001D0 .OR. DNUM.GT.1000.0D0) THEN
C                    WRITE Error Message: Particle Diameter Out-of-Range
                     CALL ERRHDL(PATH,MODNAM,'E','335',SRCID(I))
                  END IF
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
                     APDIAM(ISET,I) = DNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
               END DO
               IWRK2(I,5) = ISET

            ELSE IF (INGRP .AND. L_METHOD2(I)) THEN
C ---          Write Error Message     ! Source ID identified as Method 2
               CALL ERRHDL(PATH,MODNAM,'E','386',SRCID(I))

            END IF
         END DO SOURCE_LOOP
      END IF

 999  RETURN
      END

      SUBROUTINE INPPHI
C***********************************************************************
C                 INPPHI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Mass Fraction (PHI) Input Values
C
C        PROGRAMMER:  Jeff Wang, Roger Brode
C        MODIFIED BY: D. Strimaitis, SRC
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Mass Fraction Input Values
C
C        CALLED FROM:   PARTDEP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, IH, IL, ISDX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      FOUND  = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPPHI'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (FOUND) THEN
            ISET = IWRK2(ISDX,6)
            DO K = 4, IFC
C              Change It To Numbers
               CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  CYCLE
               END IF
               IF (DNUM .LT. 0.0D0 .OR. DNUM .GT. 1.0D0) THEN
C                 WRITE Error Message: Mass Fraction Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
               END IF
               DO J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APHI(ISET,ISDX) = DNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
               END DO
            END DO
            IWRK2(ISDX,6) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         SOURCE_LOOP: DO I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               ISET = IWRK2(I,6)
               DO K = 4, IFC
C                 Get Numbers
                  CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .EQ. -1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     CYCLE SOURCE_LOOP
                  END IF
                  IF (DNUM .LT. 0.0D0 .OR. DNUM .GT. 1.0D0) THEN
C                    WRITE Error Message: Mass Fraction Out-of-Range
                     CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(I))
                  END IF
                  DO J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APHI(ISET,I) = DNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
                  END DO
               END DO
               IWRK2(I,6) = ISET
            END IF
         END DO SOURCE_LOOP
      END IF

 999  RETURN
      END

      SUBROUTINE INPPDN
C***********************************************************************
C                 INPPDN Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Particle Density Input Values
C
C        PROGRAMMER:  D. Strimaitis, SRC
C
C        ADAPTED FROM "INPGAM"
C        PROGRAMMER:  Jeff Wang, Roger Brode
C
C        DATE:    February 15, 1993
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Particle Density Input Values
C
C        CALLED FROM:   PARTDEP
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, IH, IL, ISDX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      FOUND  = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'INPPDN'

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (FOUND) THEN
            ISET = IWRK2(ISDX,7)
            DO K = 4, IFC
C              Change It To Numbers
               CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .EQ. -1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  CYCLE
               END IF
               IF (DNUM .LE. 0.0D0) THEN
C                 WRITE Error Message: Particle Density Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','334',SRCID(ISDX))
               ELSE IF (DNUM .LE. 0.1D0) THEN
C                 WRITE Warning Message: Particle Density may be Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'W','334',SRCID(ISDX))
               END IF
               DO J = 1, IMIT
                  ISET = ISET + 1
                  IF (ISET .LE. NPDMAX) THEN
C                    Assign The Field
                     APDENS(ISET,ISDX) = DNUM
                  ELSE
C                    WRITE Error Message: Too Many PartDiam Categories
                     WRITE(DUMMY,'(I8)') NPDMAX
                     CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                  END IF
               END DO
            END DO
            IWRK2(ISDX,7) = ISET
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         SOURCE_LOOP: DO I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               ISET = IWRK2(I,7)
               DO K = 4, IFC
C                 Get Numbers
                  CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C                 Check The Numerical Field
                  IF (IMIT .NE. 1) THEN
                     CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                     CYCLE SOURCE_LOOP
                  END IF
                  IF (DNUM .LE. 0.0D0) THEN
C                    WRITE Error Message: Particle Density Out-of-Range
                     CALL ERRHDL(PATH,MODNAM,'E','334',SRCID(I))
                  ELSE IF (DNUM .LE. 0.1D0) THEN
C                    WRITE Warning Message: Particle Density may be Out-of-Range
                     CALL ERRHDL(PATH,MODNAM,'W','334',SRCID(I))
                  END IF
                  DO J = 1, IMIT
                     ISET = ISET + 1
                     IF (ISET .LE. NPDMAX) THEN
                        APDENS(ISET,I) = DNUM
                     ELSE
C                       WRITE Error Message: Too Many PartDiam Categories
                        WRITE(DUMMY,'(I8)') NPDMAX
                        CALL ERRHDL(PATH,MODNAM,'E','245',DUMMY)
                     END IF
                  END DO
               END DO
               IWRK2(I,7) = ISET
            END IF
         END DO SOURCE_LOOP
      END IF

 999  RETURN
      END

      SUBROUTINE GASDEP
C***********************************************************************
C                 GASDEP Module of ISC2 Model
C
C        PURPOSE: Processes Deposition Parameters for Gases
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    May 16, 1996
C
C        MODIFIED:   Apply range check on input parameters.
C                    R.W. Brode, MACTEC (f/k/a PES), Inc., 10/26/2004
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Dry Deposition Parameters for Gases
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IH, IL, ISDX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      FOUND  = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'GASDEP'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 7) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 7) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (FOUND) THEN
            SOGAS(ISDX) = 'Y'
C           Read Dry Deposition Parameters
C           Change Them To Numbers

C           First Get Gas Diffusivity (cm^2/s)
            CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM .LE. 0.0D0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','380','PDIFF')
            END IF
C           Assign The Field
            PDIFF(ISDX) = DNUM

CPES ---    Next Get Diffusivity in Water (cm^2/s)
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM .LE. 0.0D0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','380','PDIFFW')
            END IF
C           Assign The Field
            PDIFFW(ISDX) = DNUM

C           Now Get Lipid Cuticle Resistence for Individual Leaves (RCLI)
            CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM .LE. 0.0D0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','380','RCLI')
            END IF
C           Assign The Field
            RCLI(ISDX) = DNUM

C           Get the Henry's Law Constant
            CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            ELSE IF (DNUM .LE. 0.0D0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','380','HENRY')
            END IF
C           Assign The Field
            HENRY(ISDX) = DNUM

         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               ISDX = I
               SOGAS(ISDX) = 'Y'
C              Read Dry Deposition Parameters
C              Change Them To Numbers

C              First Get Gas Diffusivity
               CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               ELSE IF (DNUM .LE. 0.0D0) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','380','PDIFF')
               END IF
C              Assign The Field
               PDIFF(ISDX) = DNUM

CPES ---       Next Get Diffusivity in Water (cm^2/s)
               CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               ELSE IF (DNUM .LE. 0.0D0) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','380','PDIFFW')
               END IF
C              Assign The Field
               PDIFFW(ISDX) = DNUM

C              Now Get Lipid Cuticle Resistence for Individual Leaves (RCLI)
               CALL STODBL(FIELD(6),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               ELSE IF (DNUM .LE. 0.0D0) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','380','RCLI')
               END IF
C              Assign The Field
               RCLI(ISDX) = DNUM

C              Get the Henry's Law Constant
               CALL STODBL(FIELD(7),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               ELSE IF (DNUM .LE. 0.0D0) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','380','HENRY')
               END IF
C              Assign The Field
               HENRY(ISDX) = DNUM

            END IF
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE METH_2
C***********************************************************************
C                 METH_2 Module of ISC2 Model
C
C        PURPOSE: Processes Method 2 Dry Deposition Parameters for Particles
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    June 1, 2001
C
C        MODIFIED:   To check for out-of-range inputs for fine mass
C                    fraction (finemass)
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Dry Deposition Parameters for Particles using Method 2
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IH, IL, ISDX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      FOUND  = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'METH_2'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (FOUND .AND. .NOT.L_METHOD2(ISDX) .AND. 
     &                                        IWRK2(ISDX,5).EQ.0) THEN
            L_METHOD2(ISDX) = .TRUE.
C           Read Dry Deposition Parameters
C           Change Them To Numbers
C           First Get Mass Fraction of Fine Particles (.lt. 2.5 microns)
            CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            IF (DNUM .LT. 0.0D0 .OR. DNUM .GT. 1.0D0) THEN
C              WRITE Error Message: Mass Fraction Out-of-Range
               CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
            ELSE
C              Assign The Field
               FINEMASS(ISDX) = DNUM
            END IF

C           Now Get Mass Mean Diameter
            CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
C           Assign The Field
            APDIAM(1,ISDX) = DNUM

C           Set mass fraction and particle density
            APHI(1,ISDX)   = 1.0D0
            APDENS(1,ISDX) = 1.0D0

C           Set number of particle size categories to 1
            INPD(ISDX) = 1

         ELSE IF (FOUND .AND. L_METHOD2(ISDX)) THEN
C ---       Write Error Message     ! Source ID identified twice
            CALL ERRHDL(PATH,MODNAM,'E','387',SRCID(ISDX))

         ELSE IF (FOUND .AND. IWRK2(ISDX,5) .GT. 0) THEN
C ---       Write Error Message     ! Source ID identified as Method 1
            CALL ERRHDL(PATH,MODNAM,'E','386',SRCID(ISDX))

         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
               ISDX = I
               IF (.NOT.L_METHOD2(ISDX) .AND. IWRK2(ISDX,5) .EQ. 0) THEN
                  L_METHOD2(ISDX) = .TRUE.
               ELSE IF (L_METHOD2(ISDX)) THEN
C ---             Write Error Message     ! Source ID identified twice
                  CALL ERRHDL(PATH,MODNAM,'E','387',SRCID(ISDX))
                  CYCLE
               ELSE IF (IWRK2(ISDX,5) .GT. 0) THEN
C ---             Write Error Message     ! Source ID identified as Method 1
                  CALL ERRHDL(PATH,MODNAM,'E','386',SRCID(ISDX))
                  CYCLE
               END IF
C              Read Dry Deposition Parameters
C              Change Them To Numbers
C              First Get Mass Fraction of Fine Particles (.lt. 2.5 microns)
               CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 999
               END IF
               IF (DNUM .LT. 0.0D0 .OR. DNUM .GT. 1.0D0) THEN
C                 WRITE Error Message: Mass Fraction Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','332',SRCID(ISDX))
               ELSE
C                 Assign The Field
                  FINEMASS(ISDX) = DNUM
               END IF

C              Now Get Mass Mean Diameter
               CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 999
               END IF
C              Assign The Field
               APDIAM(1,ISDX) = DNUM

C              Set mass fraction and particle density
               APHI(1,ISDX)   = 1.0D0
               APDENS(1,ISDX) = 1.0D0

C              Set number of particle size categories to 1
               INPD(ISDX) = 1

            END IF
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE SOGRP
C***********************************************************************
C                 SOGRP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Source Group Inputs for Pass One
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Source Group Input For Pass One
C
C        CALLED FROM: SOCARD
C***********************************************************************
C

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, K, IH, IL
      CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
      LOGICAL CONT, INGRP, RMARK

C     Variable Initializations
      CONT   = .FALSE.
      MODNAM = 'SOGRP'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LE. 3 .AND. FIELD(3) .NE. 'ALL') THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     READ in the Group ID and Check for Continuation Card
      TEMPID = FIELD(3)
      DO I = 1, NUMGRP
         IF (TEMPID .EQ. GRPID(I)) THEN
            CONT = .TRUE.
         END IF
      END DO

C     Increment Counters and Assign Group ID If Not a Continuation Card
      IF (.NOT. CONT) THEN
         IGRP = IGRP + 1
         IF (IGRP .GT. NGRP) THEN
C           WRITE Error Message    ! Too Many Source Groups Specified
            WRITE(DUMMY,'(I8)') NGRP
            CALL ERRHDL(PATH,MODNAM,'E','235',DUMMY)
C           Exit to END
            GO TO 999
         END IF
         NUMGRP = NUMGRP + 1
         GRPID(IGRP) = TEMPID
      END IF

C     Set Up The Source Group Array
      IF (GRPID(IGRP) .EQ. 'ALL' .AND. .NOT.CONT) THEN
         DO I = 1, NUMSRC
            IGROUP(I,IGRP) = 1
         END DO
         IF (FIELD(4) .EQ. 'BACKGROUND') THEN
            GRP_BACK(IGRP) = .TRUE.
         END IF
      ELSE
C        Loop Through Fields
         DO I = 4, IFC
            IF (FIELD(I) .EQ. 'BACKGROUND') THEN
               GRP_BACK(IGRP) = .TRUE.
               CYCLE
            END IF
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,
     &                  LOWID,HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               CYCLE
            END IF
            DO K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  IGROUP(K,IGRP) = 1
               END IF
            END DO
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE ASNGRP(INID,LOWID,HIGID,INGRP)
C***********************************************************************
C                 ASNGRP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Find Whether A Source ID is In The Specific Group
C
C        PROGRAMMER: Roger Brode, Jeff Wang, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Field Parameters
C
C        OUTPUTS: Indicator for Source ID in The Group
C
C        CALLED FROM: (This is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      CHARACTER (LEN=12) :: LOWID, HIGID, INID, IID1, LID1, HID1, 
     &                      IID2, LID2, HID2
      CHARACTER PATH*2, MODNAM*12
      INTEGER IN, IL, IH
      LOGICAL INGRP

C     Variable Initializations
      MODNAM = 'ASNGRP'
      PATH   = 'SO'
      INGRP  = .FALSE.

C     Extract The Character Field And Numerical Field
      CALL SETIDG(INID,IID1,IN,IID2)
      CALL SETIDG(LOWID,LID1,IL,LID2)
      CALL SETIDG(HIGID,HID1,IH,HID2)

C     Do Comparisons of Character and Numeric Fields, All Must Satisfy Ranges
      IF ((IID1.GE.LID1 .AND. IID1.LE.HID1) .AND.
     &        (IN.GE.IL .AND. IN.LE.IH) .AND.
     &    (IID2.GE.LID2 .AND. IID2.LE.HID2)) THEN
         INGRP = .TRUE.
      END IF

      RETURN
      END

      SUBROUTINE SETIDG(INID,IDCHR1,IDNUM,IDCHR2)
C***********************************************************************
C                 SETIDG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Find A Source ID's Character Part and
C                 Numerical Part
C
C        PROGRAMMER: Jeff Wang, Roger Brode, Kevin Stroupe
C
C        DATE:    March 2, 1992
C
C        REVISION HISTORY:
C
C                 Modified internal read from NUMID to use 'I8'
C                 format to avoid runtime errors with some compilers.
C                 R.W. Brode, U.S. EPA/OAQPS/AQMG, 01/26/2007
C
C                 Modified conversion of numeric portion to use internal
C                 read rather than using call to STONUM in order to
C                 avoid precision problems for 8-digit integer IDs.
C                 R. Brode, PES, 8/9/01
C
C        INPUTS:  Input Field Parameters
C
C        OUTPUTS: An Initial Character String, a Number, and
C                 a Second Character String
C
C        CALLED FROM: (This is An Utility Program)
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, II, ISTR, IDNUM
      CHARACTER (LEN=12) :: INID, IDCHR1, IDCHR2
      CHARACTER (LEN= 1) :: CHKI
      CHARACTER (LEN=ILEN_FLD) :: NUMID
      LOGICAL HIT

C     Variable Initializations
      MODNAM = 'SETIDG'
      I  = 12
      II = 0
      NUMID  = ' '
      IDCHR1 = ' '
      IDCHR2 = ' '
      IDNUM  = 0
      HIT    = .FALSE.

C     Find The Length of the Input Field, II (<= 12)
      DO WHILE (.NOT.HIT .AND. I.GE.1)
         CHKI = INID(I:I)
         IF (CHKI .NE. ' ') THEN
            II = I
            HIT = .TRUE.
         END IF
         I = I - 1
      END DO

C     Divide the Input Id into 3 parts (char1, int, and char2)
      I = 1
      ISTR = I
      CHKI = INID(I:I)
C     Get first character part
      DO WHILE (CHKI .LT. '0' .OR. CHKI .GT. '9')
         IDCHR1 = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

C     Get integer part
      ISTR = I
      DO WHILE (CHKI .GE. '0' .AND. CHKI .LE. '9')
         NUMID = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

C     Get second character part
      ISTR = I
      DO WHILE (I .LE. II)
         IDCHR2 = INID(ISTR:I)
         I = I + 1
         IF (I .GT. II) THEN
            GO TO 20
         ELSE
            CHKI = INID(I:I)
         END IF
      END DO

 20   CONTINUE

C     Convert Numeric Part to Integer Variable
      READ(NUMID,'(I8)') IDNUM

      RETURN
      END

c----------------------------------------------------------------------
      subroutine vdp1
c----------------------------------------------------------------------
c
c --- ISC2ST     Version:  1.0     Level:  930215                  VDP1
c                J. Scire, SRC
c
c --- PURPOSE:  Setup routine for PARTICLE dry deposition.
c               Completes particle common block /SOURC4/.  Performs
c               initialization and time-invariant calculations.
c
c --- MODIFIED: To require non-zero values for all gas deposition
c               parameters.
c               R. W. Brode, MACTEC (f/k/a PES), Inc., 10/26/2004
c
c --- MODIFIED: To include calculation of SCF and AVGRAV for
c               Method 2 sources, remove assignment of ZRDEP,
c               and to remove unused calculations.
c               R. W. Brode, MACTEC (f/k/a PES), Inc., 03/19/04
c
c --- MODIFIED: To save SCF values in array for use in SUB. VDP for
c               new TOXICS deposition option
c               R. W. Brode, PES, Inc., 02/11/03
c
c --- MODIFIED: Set deposition reference height, ZRDEP, to 1.0 meter.
c               R. W. Brode, PES, Inc., 12/29/97
c
c --- INPUTS:
c     Common block /SOURC4/ variables:
c              INPD - integer    - Number of particle size categories
c            APDIAM - real array - Mean diameter (microns) of each
c                                  particle size category
c              APHI - real array - Mass fraction in each size category
c            APDENS - real       - Particle density (g/cm**3)
c
c --- OUTPUT:
c     Common block /SOURC4/ variables:
c            AVGRAV - real array - Gravitational settling velocity (m/s)
c            ATSTOP - real array - Stopping time (s)
c            VAIRMS - real       - Viscosity of air (m**2/s)
c            VDPHOR - real       - Phoretic effects term (m/s)
c
c --- VDP1 called by:  SOCARD
c --- VDP1 calls:      none
c----------------------------------------------------------------------
c
      USE MAIN1

      DOUBLE PRECISION, PARAMETER :: a1=1.257D0, a2=0.4D0, a3=0.55D0, 
     &                   xmfp=6.5D-6,
     &                   vcon=1.81D-4, xk=1.38D-16, vair=0.15D0,
     &                   gcgs=981.0D0, rhoair=1.2D-3, tair=293.15D0
      DOUBLE PRECISION :: DIAMCM
      INTEGER          :: I, J, N, IO6
c
      io6=iounit
c ***
      if(DEBUG)then
         write(io6,*)
         write(io6,*)'SUBR. VDP1 -- INPUTS'
         write(io6,*)
         do i=1,numsrc
          write(io6,*)'SOURCE          = ',i
          write(io6,*)'INPD            = ',inpd(i)
          if (inpd(i) .gt. 0) then
           write(io6,*)'APDIAM (um)     = ',(apdiam(n,i),n=1,inpd(i))
           write(io6,*)'APHI            = ',(aphi(n,i),n=1,inpd(i))
           write(io6,*)'APDENS(g/cm**3) = ',(apdens(n,i),n=1,inpd(i))
          end if
          write(io6,*)
         end do
      endif
c ***
c
c --- Convert viscosity of air (at 20 deg C) from cm**2/s to m**2/s
      vairms=1.0D-4*vair
c
c --- Define phoretic effects term (m/s)
      vdphor=0.0001D0
c
c
c --  LOOP over sources
      do j=1,numsrc
c
         if(inpd(j) .LE. 0 .and. .not.luservd) then
c
            if (pdiff(j).ne.0.0D0 .and. pdiffw(j).ne.0.0D0
     &          .and. rcli(j).ne.0.0D0 .and. henry(j).ne.0.0D0) then
c ---          GAS DEPOSITION
c
c ---          Convert Pollutant diffusivity (cm**2/s to m**2/s)
               pdiff(j) =pdiff(j)*1.0D-4
               pdiffw(j)=pdiffw(j)*1.0D-4
c
c ---          Convert rcli resistance from s/cm to s/m
               rcli(j)=rcli(j)*1.0D2
c
c ***
               if(debug)then
                 write(io6,*)
                 write(io6,*)'SUBR. VDP1 -- OUTPUT for GASES'
                 write(io6,*)'PDIFF (m**2/s)  = ',(pdiff(n),n=1,numsrc)
                 write(io6,*)'PDIFFW(m**2/s)  = ',(pdiffw(n),n=1,numsrc)
                 write(io6,*)'RCLI(s/m)       = ',(rcli(n),n=1,numsrc)
                 write(io6,*)'ZRDEP (m)       = ',zrdep
               endif
c ***
            endif
            
         else if (inpd(j) .GT. 0) then
c
c ---       PARTICLE DEPOSITION
c
c ---       LOOP over "INPD" size intervals if non-zero
c
            do i=1,inpd(j)
c ---          Slip correction factor
               diamcm=1.0D-4*apdiam(i,j)
               scf(i)=1.0D0+2.0D0*xmfp*
     &               (a1+a2*DEXP(-a3*diamcm/xmfp))/diamcm
c
c ---          Gravitational settling velocity (m/s)
c ---          (rhoair is approx. density of air -- 1.2e-3 g/cm**3)
crwb           Use PARAMETER G = 9.80616 m/s^2 instead of gcgs = 981 cm/s^2
crwb           avgrav(i,j)=0.01*(apdens(i,j)-rhoair)*gcgs*diamcm**2
c ---          Set lower limit of 0.0 on density difference
               avgrav(i,j)= MAX(0.0D0,(apdens(i,j)-rhoair))*G*diamcm**2
     &                        *scf(i)/(18.0D0*vcon)
c
c ---          Stopping times
crwb           Use PARAMETER G = 9.80616 m/s^2 instead of gcgs = 981 cm/s^2
crwb           atstop(i,j)=avgrav(i,j)/(0.01*gcgs)
               atstop(i,j)=avgrav(i,j)/G
            end do
c ***
            if(DEBUG)then
             write(io6,*)
             write(io6,*)'SUBR. VDP1 -- OUTPUT for PARTICLES'
             write(io6,*)
             do i=1,numsrc
              write(io6,*)'SOURCE          = ',i
              write(io6,*)'AVGRAV (m/s)    = ',(avgrav(n,i),n=1,inpd(i))
              write(io6,*)'ATSTOP (s)      = ',(atstop(n,i),n=1,inpd(i))
              write(io6,*)'VAIRMS (m**2/s) = ',vairms
              write(io6,*)'ZRDEP (m)       = ',zrdep
              write(io6,*)'VDPHOR (m/s)    = ',vdphor
              write(io6,*)
             end do
            endif
c ***
c
         endif
      end do
c     end LOOP over source

      return
      end

      SUBROUTINE HREMIS
C***********************************************************************
C                 HREMIS Module of AERMOD
C
C        PURPOSE: To process Hourly Emissions Data
C
C        PROGRAMMER: Jayant Hardikar, Roger Brode
C
C        DATE:    September 15, 1993
C
C        INPUTS:  Pathway (SO) and Keyword (HOURLY)
C
C        OUTPUTS: Source QFLAG Array
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, K, IH, IL

      LOGICAL FOPEN, INGRP
      LOGICAL RMARK

      CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID

C     Variable Initializations
      MODNAM = 'HREMIS'

      FOPEN  = .FALSE.

      IF (IFC .GE. 4) THEN
C        Retrieve Hourly Emissions Data Filename as Character Substring to
C        Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            HRFILE = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  HRFILE Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            GO TO 999
         END IF

C        Open Hourly Emissions Data File If Not Already Open
         INQUIRE (FILE=HRFILE,OPENED=FOPEN)

         IF (.NOT. FOPEN) THEN
C           Open Hourly Emissions Data File If Not Already Open
C           Open with ACTION='READ' to prevent overwrite and allow multiple access
            INQUIRE (UNIT=IHREMI,OPENED=FOPEN)
            IF (.NOT. FOPEN) THEN
               OPEN(UNIT=IHREMI,ERR=998,FILE=HRFILE,IOSTAT=IOERRN,
     &              ACTION='READ',STATUS='OLD')
            ELSE
C              Hourly Emissions File is Already Opened With Different Filename
               CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)
               GO TO 999
            END IF
         END IF

      ELSE
C        WRITE Error Message         ! Not Enough Parameters Specified
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

      TEMPID = FIELD(4)

C     Set Up The Source Group Array
      IF (TEMPID .EQ. 'ALL') THEN
         DO I = 1, NUMSRC
            QFLAG(I) = 'HOURLY'
         END DO
      ELSE
C        Loop Through Fields
         DO I = 4, IFC
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,
     &                  LOWID,HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               CYCLE
            END IF
            DO K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  QFLAG(K) = 'HOURLY'
               END IF
            END DO
         END DO
      END IF

      GO TO 999

C     Process Error Messages
998   CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)

999   RETURN
      END

      SUBROUTINE URBANS
C***********************************************************************
C                 URBANS Module of AERMOD Model
C
C        PURPOSE: Processes Urban Source Card
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    June 11, 1996
C
C        MODIFIED:   Modified to allow for the use of URBANSRC ALL on
C                    the SO pathway to indicate that all sources are 
C                    to be treated as URBAN sources. This option assumes 
C                    that only one (1) urban area has been defined using 
C                    the CO URBANOPT keyword.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/29/2012
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Array of flags for Urban Sources
C
C        CALLED FROM: SOCARD
C***********************************************************************
C

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
      INTEGER :: I, IL, IH, K, ISTR
      LOGICAL INGRP, RMARK, FOUND

C     Variable Initializations
      MODNAM = 'URBANS'
      FOUND = .FALSE.

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF

C     Check for 'URBANSRC ALL' option identified in PRESET
      IF (L_URBAN_ALL) THEN
         IF (IFC .EQ. 3 .AND. FIELD(3) .EQ. 'ALL') THEN
            URBSRC(:) = 'Y'
            IURBGRP(:,:) = 1
            GO TO 999
         ELSE
C           WRITE Error Message:  URBANSRC ALL 
            CALL ERRHDL(PATH,MODNAM,'E','279','URBANSRC ALL')
            GO TO 999
         END IF

      ELSE IF (L_MULTURB) THEN
C        Multiple Urban Areas
C        READ in the Group ID and Check for Continuation Card
         TEMPID = FIELD(3)
         DO I = 1, NUMURB
            IF (TEMPID .EQ. URBID(I)) THEN
               FOUND = .TRUE.
               IURB = I
            END IF
         END DO
         IF (.NOT. FOUND) THEN
C           WRITE Error Message:  Urban ID not defined
            CALL ERRHDL(PATH,MODNAM,'E','301',TEMPID)
            GO TO 999
         END IF

C        Specify field index to start for Source IDs
         ISTR = 4

      ELSE
C        Single Urban Area - No URBAN ID
         IURB = 1

C        Specify field index to start for Source IDs
         ISTR = 3

      END IF

C     Loop Through Fields
      DO I = ISTR, IFC
         IF (INDEX(FIELD(I),'-') .EQ. 0) THEN
            FOUND = .FALSE.
            DO K = 1, NUMSRC
               IF (SRCID(K) .EQ. FIELD(I)) THEN
                  FOUND = .TRUE.
                  URBSRC(K) = 'Y'
                  IURBGRP(K,IURB) = 1
               END IF
            END DO
            IF (.NOT.FOUND) THEN
C              WRITE Error Message:  SRCID not found
               CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
               CYCLE
            END IF
            
         ELSE

            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,LOWID,
     &                  HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               CYCLE
            END IF
            DO K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  URBSRC(K) = 'Y'
                  IURBGRP(K,IURB) = 1
               END IF
            END DO
            
         END IF
      END DO

 999  RETURN
      END

      SUBROUTINE NO2RAT
C***********************************************************************
C                 NO2RAT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes In-stack NO2/NOX Ratios by Source for
C                 OLM and PVMRM Options
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:    May 6, 2002
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Array of in-stack NO2/NOX ratios
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IH, IL, ISDX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      FOUND  = .FALSE.
      INGRP  = .FALSE.
      MODNAM = 'NO2RAT'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 4) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Get The Source ID(s)
      SOID = FIELD(3)
      CALL FSPLIT(PATH,KEYWRD,SOID,ILEN_FLD,'-',RMARK,LID,HID)

      IF (LID .EQ. HID) THEN
C        Search For The Index
         CALL SINDEX(SRCID,NSRC,SOID,ISDX,FOUND)
         IF (FOUND) THEN
C           Read NO2/NOX Ratio and Convert to Real
            CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C           Check The Numerical Field
            IF (IMIT .NE. 1) THEN
               CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               GO TO 999
            END IF
            IF (DNUM .LT. 0.0D0 .OR. DNUM .GT. 1.0D0) THEN
C              WRITE Error Message: NO2_Ratio Out-of-Range
               CALL ERRHDL(PATH,MODNAM,'E','336',SRCID(ISDX))
            END IF
C           Assign The Field
            ANO2_RATIO(ISDX) = DNUM
         ELSE
C           WRITE Error Message     ! Source Location Has Not Been Identified
            CALL ERRHDL(PATH,MODNAM,'E','300',KEYWRD)
         END IF
      ELSE
C        First Check Range for Upper Value < Lower Value
         CALL SETIDG(LID,LID1,IL,LID2)
         CALL SETIDG(HID,HID1,IH,HID2)
         IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C           WRITE Error Message:  Invalid Range,  Upper < Lower
            CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
            GO TO 999
         END IF
         DO I = 1, NUMSRC
C           See Whether It's In The Group
            CALL ASNGRP(SRCID(I),LID,HID,INGRP)
            IF (INGRP) THEN
C              Read NO2/NOX Ratio and Convert to Real
               CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C              Check The Numerical Field
               IF (IMIT .NE. 1) THEN
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
                  GO TO 999
               END IF
               IF (DNUM .LT. 0.0D0 .OR. DNUM .GT. 1.0D0) THEN
C                 WRITE Error Message: NO2_Ratio Out-of-Range
                  CALL ERRHDL(PATH,MODNAM,'E','336',SRCID(I))
               END IF
C              Assign The Field
               ANO2_RATIO(I) = DNUM
            END IF
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE OLMGRP
C***********************************************************************
C                 OLMGRP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes OLM Source Group Inputs
C
C        PROGRAMMER: Roger W. Brode, PES, Inc.
C
C        DATE:    May 6, 2002
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: OLM Source Group Inputs
C
C        CALLED FROM: SOCARD
C***********************************************************************
C

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, K, IH, IL
      CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
      LOGICAL CONT, INGRP, RMARK

C     Variable Initializations
      CONT   = .FALSE.
      MODNAM = 'OLMGRP'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LE. 3 .AND. FIELD(3) .NE. 'ALL') THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C     READ in the Group ID and Check for Continuation Card
      TEMPID = FIELD(3)
      DO I = 1, NUMOLM
         IF (TEMPID .EQ. OLMID(I)) THEN
            CONT = .TRUE.
         END IF
      END DO

C     Increment Counters and Assign Group ID If Not a Continuation Card
      IF (.NOT. CONT) THEN
         IOLM = IOLM + 1
         IF (IOLM .GT. NOLM) THEN
C           WRITE Error Message    ! Too Many OLM Groups Specified
            WRITE(DUMMY,'(I8)') NOLM
            CALL ERRHDL(PATH,MODNAM,'E','281',DUMMY)
C           Exit to END
            GO TO 999
         END IF
         NUMOLM = NUMOLM + 1
         OLMID(IOLM) = TEMPID
      END IF

C     Set Up The Source Group Array -
      IF (OLMID(IOLM) .EQ. 'ALL' .AND. .NOT.CONT) THEN
C ---    Check for whether all sources have been defined yet, 
C        based on NSRC determined during PRESET vs. NUMSRC
C        determined during normal SETUP.
         IF (NUMSRC .LT. NSRC) THEN
C           Issue fatal error message; OLMGROUP ALL out of order
            CALL ERRHDL(PATH,MODNAM,'E','140','OLMGROUP ALL')
            GO TO 999
         ELSE
            DO I = 1, NUMSRC
               IGRP_OLM(I,IOLM) = 1
               L_OLMGRP(I) = .TRUE.
            END DO
         END IF
      ELSE
C        Loop Through Fields
         DO I = 4, IFC
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,
     &                  LOWID,HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','SRCRANGE')
               CYCLE
            END IF
            DO K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  IGRP_OLM(K,IOLM) = 1
                  L_OLMGRP(K) = .TRUE.
               END IF
            END DO
         END DO
      END IF

 999  RETURN
      END

      SUBROUTINE PSDGRP
C***********************************************************************
C                 PSDGRP Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes PSD Source Group Inputs
C
C        PROGRAMMER: Jim Paumier, MACTEC FPI
C        Based on code for SOGRP and OLMGRP by: Roger W. Brode, PES, Inc.
C
C        DATE:    September 30, 2006
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: PSD Source Group Inputs
C
C        CALLED FROM: SOCARD
C***********************************************************************
C

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, K, IH, IL
      CHARACTER (LEN=12) :: LOWID, HIGID, LID1, LID2, HID1, HID2, TEMPID
      LOGICAL CONT, INGRP, RMARK

C     Variable Initializations
      CONT   = .FALSE.
      MODNAM = 'PSDGRP'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
C        Note:  The ALL parameter is not valid for the PSDCREDIT option
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      END IF

C     READ in the Group ID and Check if valid name
      TEMPID = FIELD(3)
      DO I = 1, NUMPSD
         IF (TEMPID .EQ. PSDID(I)) THEN
            CONT = .TRUE.
         END IF
      END DO

C     Increment Counters and Assign Group ID If Not a Continuation Card
      IF (.NOT. CONT) THEN
         IPSD = IPSD + 1
         IF (IPSD .GT. NPSD) THEN
C           WRITE Error Message    ! Too Many PSD Groups Specified
            WRITE(DUMMY,'(I8)') NPSD
            CALL ERRHDL(PATH,MODNAM,'E','281',DUMMY)
C           Exit to END
            GO TO 999
         END IF
         NUMPSD = NUMPSD + 1
         PSDID(IPSD) = TEMPID
      END IF

C     Set Up The Source Group Array and the PSD source type
C       Only INCRCONS (increment consuming), RETRBASE (retired baseline), 
C       and NONRBASE (non-retired basleine) are allowable; any other
C       PSD group name, including ALL, is not allowed
      IF( PSDID(IPSD) .EQ. 'INCRCONS' .OR.
     &    PSDID(IPSD) .EQ. 'RETRBASE' .OR.
     &    PSDID(IPSD) .EQ. 'NONRBASE' )THEN

C        Valid PSD Source Group; Loop Through Fields
         DO I = 4, IFC
            CALL FSPLIT(PATH,KEYWRD,FIELD(I),ILEN_FLD,'-',RMARK,
     &                  LOWID,HIGID)
C           First Check Range for Upper Value < Lower Value
            CALL SETIDG(LOWID,LID1,IL,LID2)
            CALL SETIDG(HIGID,HID1,IH,HID2)
            IF ((HID1.LT.LID1) .OR. (IH.LT.IL) .OR. (HID2.LT.LID2)) THEN
C              WRITE Error Message:  Invalid Range,  Upper < Lower
               CALL ERRHDL(PATH,MODNAM,'E','203','PSDRANGE')
               CYCLE
            END IF
            DO K = 1, NUMSRC
               CALL ASNGRP(SRCID(K),LOWID,HIGID,INGRP)
               IF (INGRP) THEN
                  IGRP_PSD(K,IPSD) = 1
                  L_PSDGRP(K) = .TRUE.
                  IF( PSDID(IPSD) .EQ. 'INCRCONS' )THEN
                      PSDSRCTYP(K) = 'IC'
                  ELSE IF( PSDID(IPSD) .EQ. 'NONRBASE' )THEN
                      PSDSRCTYP(K) = 'NB'
                  ELSE IF( PSDID(IPSD) .EQ. 'RETRBASE' )THEN
                      PSDSRCTYP(K) = 'RB'
                  END IF
               END IF
            END DO
         END DO
      ELSE
C        Error Message: Not a valid PSD source group
         CALL ERRHDL(PATH,MODNAM,'E','287',KEYWRD)
         GO TO 999
      END IF

 999  RETURN
      END

      SUBROUTINE BACK_GRND
C***********************************************************************
C                 BACK_GRND Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes User-specified BACKGROUND concentrations, 
C                 based on same options for temporal variability 
C                 as the EMISFACT keyword for source emissions
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:       February 28, 2011
C
C        MODIFIED: Include checks for potential problem with Fortran
C                  format specifier.  Should include from 1 to 4 
C                  integers for date variables, and one real for 
C                  background data variable.  Warning message is issued
C                  if too many or too few integers/reals are specified.
C                  An error message may also be issued when reading
C                  the background file depending on the compiler options.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 04/13/2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IH, IL, ISDX, IBMAX
      INTEGER :: NumInt, NumReal
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      CHARACTER (LEN=8) :: TMP_BFLAG
      LOGICAL FOUND, INGRP, RMARK, FOPEN

      SAVE TMP_BFLAG

C     Variable Initializations
      MODNAM = 'BACK_GRND'
      DATA TMP_BFLAG/' '/
      
C --- Assign logical variable indicating that background concentrations
C     are specified
      L_BACKGRND = .TRUE.
      FOPEN  = .FALSE.
      NumInt  = 0
      NumReal = 0

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .EQ. 3) THEN
C        Error Message: No Numerical Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 4) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      END IF

C --- Check for inconsistency in BACKGRND options
      IF (LEN_TRIM(TMP_BFLAG) .GT. 0 .AND. 
     &      (TMP_BFLAG .NE. 'HOURLY' .AND. 
     &        FIELD(3) .NE. 'HOURLY' .AND.
     &        FIELD(3) .NE. TMP_BFLAG) )THEN
C ---    More than one temporally-varying background
C        option has been specified (other than HOURLY).
C        Error Message: Inconsistent background flag
         CALL ERRHDL(PATH,MODNAM,'E','165',FIELD(3))
         GO TO 999
      ELSE IF (LEN_TRIM(BFLAG) .GT. 0 .AND. 
     &         FIELD(3) .NE. 'HOURLY' .AND.
     &         FIELD(3) .NE. BFLAG) THEN
C ---    More than one temporally-varying background
C        option has been specified (other than HOURLY).
C        Error Message: Inconsistent background flag
         CALL ERRHDL(PATH,MODNAM,'E','165',FIELD(3))
         GO TO 999
      ELSE
         TMP_BFLAG = FIELD(3)
      END IF
      
C --- First check for hourly background file
      IF (TMP_BFLAG .EQ. 'HOURLY') THEN
C ---    Check for L_HourlyBackgrnd = .TRUE., indicating that
C        BACKGRND HOURLY has already been specified
         IF (L_HourlyBackgrnd) THEN
C           WRITE Error Message:  Duplicate HOURLY background keyword
            CALL ERRHDL(PATH,MODNAM,'E','166','SO pathway')
            GO TO 999
         END IF

C ---    Hourly background concentration option selected;
C        set logical flag and get filename
         L_HourlyBackgrnd = .TRUE.
         
         IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            BKGRND_File = RUNST1(LOCB(4):LOCE(4))
         ELSE
C           WRITE Error Message:  BKGRND_File Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            GO TO 999
         END IF

C        Open Hourly Emissions Data File If Not Already Open
         INQUIRE (FILE=BKGRND_File,OPENED=FOPEN)

         IF (.NOT. FOPEN) THEN
C           Open Hourly Emissions Data File If Not Already Open
C           Open with ACTION='READ' to prevent overwrite and allow multiple access
            INQUIRE (UNIT=IBGUNT,OPENED=FOPEN)
            IF (.NOT. FOPEN) THEN
               OPEN(UNIT=IBGUNT,ERR=998,FILE=BKGRND_File,IOSTAT=IOERRN,
     &              ACTION='READ',STATUS='OLD')
            ELSE
C              Hourly Emissions File is Already Opened With Different Filename
               CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)
               GO TO 999
            END IF
         END IF

C ---    Check for optional hourly BACKGRND file format
         IF (IFC .EQ. 5) THEN
C           Check for Format String > ILEN_FLD PARAMETER
            IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1)) THEN
C              Retrieve Met Format as Char. Substring 
               BGFORM = RUNST1(LOCB(5):LOCE(5))
C ---          Check for correct format specifiers for BACKGRND file;
C              should be 4 integers for date variables and 1 real for
C              background concentration; allow for 1 to 4 integers since
C              format statement may include 4I2, and also allow for 
C              either F, E, or D format for the data variable.
               DO I = 1, LEN_TRIM(BGFORM)
                  IF (BGFORM(I:I).EQ.'I' .OR. 
     &                BGFORM(I:I).EQ.'i') THEN
                     NumInt  = NumInt  + 1
                  ELSE IF (BGFORM(I:I).EQ.'F' .OR. 
     &                     BGFORM(I:I).EQ.'f') THEN
                     NumReal = NumReal + 1
                  ELSE IF (BGFORM(I:I).EQ.'E' .OR. 
     &                     BGFORM(I:I).EQ.'e') THEN
                     NumReal = NumReal + 1
                  ELSE IF (BGFORM(I:I).EQ.'D' .OR. 
     &                     BGFORM(I:I).EQ.'d') THEN
                     NumReal = NumReal + 1
                  END IF
               END DO
               IF (NumInt.LT.1 .OR. NumInt.GT.4) THEN
C                 WRITE Warning Message:  Potential problem with BGFORM
                  WRITE(DUMMY,'(''NumInts= '',I3)') NumInt
                  CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
               END IF
               IF (NumReal.NE.1) THEN
C                 WRITE Warning Message:  Potential problem with BGFORM
                  WRITE(DUMMY,'(''NumReal= '',I3)') NumReal
                  CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
               END IF
            ELSE
C              WRITE Error Message:  BGFORM Field is Too Long
               WRITE(DUMMY,'(''LEN='',I6)') LOCE(5)-LOCB(5)
               CALL ERRHDL(PATH,MODNAM,'E','292',DUMMY)
            END IF
         ELSE IF (IFC .EQ. 4) THEN
C ---       Use 'free' format as the default
            BGFORM = 'FREE'
         ELSE IF (IFC .GT. 5) THEN
C ---       Error Message: Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
            GO TO 999
         END IF

      ELSE
      
         BFLAG = TMP_BFLAG
         
         IF (BFLAG .EQ. 'ANNUAL') THEN
            IBMAX = 1
         ELSE IF (BFLAG .EQ. 'SEASON') THEN
            IBMAX = 4
         ELSE IF (BFLAG .EQ. 'MONTH') THEN
            IBMAX = 12
         ELSE IF (BFLAG .EQ. 'HROFDY') THEN
            IBMAX = 24
         ELSE IF (BFLAG .EQ. 'WSPEED') THEN
            IBMAX = 6
         ELSE IF (BFLAG .EQ. 'SEASHR') THEN
            IBMAX = 96
         ELSE IF (BFLAG .EQ. 'HRDOW') THEN
            IBMAX = 72
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (BFLAG .EQ. 'HRDOW7') THEN
            IBMAX = 168
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (BFLAG .EQ. 'SHRDOW') THEN
            IBMAX = 288
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (BFLAG .EQ. 'SHRDOW7') THEN
            IBMAX = 672
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (BFLAG .EQ. 'MHRDOW') THEN
            IBMAX = 864
            L_DayOfWeekOpts = .TRUE.
         ELSE IF (BFLAG .EQ. 'MHRDOW7') THEN
            IBMAX = 2016
            L_DayOfWeekOpts = .TRUE.
         ELSE
C           WRITE Error Message    ! Invalid QFLAG Field Entered
            CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
            GO TO 999 
         END IF
      
         IF (IBMAX .LE. NBF) THEN
            CALL BGFILL(IBMAX)
         ELSE IF (IBMAX .GT. NBF) THEN
C           WRITE Error Message     ! NBF Parameter Not Large Enough
            WRITE(DUMMY,'(''NBF ='',I8)') NBF
            CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
         END IF

      END IF
      
      GO TO 999

C     Process Error Messages; error opening file
 998  CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)

 999  RETURN
      END

      SUBROUTINE BGFILL(IBMAX)
C***********************************************************************
C                 BGFILL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Fill Variable Background Concentration Array
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   BACK_GRND
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J, K, IBMAX

C     Variable Initializations
      MODNAM = 'BGFILL'

      ISET = IBKGRD

      DO K = 4, IFC
C        Change Fields To Numbers
         CALL STODBL(FIELD(K),ILEN_FLD,DNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. IBMAX) THEN
               BACKGRND(ISET) = DNUM
               IF (DNUM .LT. 0.0D0) THEN
C                 WRITE Error Message:  Negative Value for BACKGRND
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many BACKGRND Values Input
               CALL ERRHDL(PATH,MODNAM,'E','231','BACKGRND')
            END IF
         END DO
      END DO

      IBKGRD = ISET

      RETURN
      END

      SUBROUTINE BACK_UNIT
C***********************************************************************
C                 BACK_UNIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes user-specified units for BACKGROUND 
C                 concentrations, 
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   SOCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'BACK_UNIT'
      
C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Check for units of background values
      IF (FIELD(3).EQ.'PPM' .OR. FIELD(3).EQ.'PPB' .OR.
     &    FIELD(3).EQ.'UG/M3') THEN
         BackUnits = FIELD(3)
      ELSE
C        Write Error Message:  Invalid units for background values
         CALL ERRHDL(PATH,MODNAM,'E','203','BackUnits')
      END IF

 999  RETURN
      END

