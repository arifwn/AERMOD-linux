      SUBROUTINE COCARD
C***********************************************************************
C                 COCARD Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To process COntrol Pathway card images
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Added the PSDCREDIT option for PVMRM; in this release
C                    specifying PSDCREDIT also requires specifying PVMRM;
C                    specifying PSDCREDIT and OLM is not a valid combination
C                    J Paumier, MACTEC -  09/30/2006
C
C        MODIFIED:   Added undocumentd NODRYDPLT and NOWETDPLT options to
C                    MODOPS header.  Also moved code to write header of
C                    DEBUG output file to AERMOD.FOR to follow SETUP,
C                    to accommodate final setting for DRYDPLT and WETDPLT.
C                    R. W. Brode, PES - 10/26/2004
C
C        MODIFIED:   To allow 24-hour or ANNUAL averages to be modeled
C                    separately for post-1997 PM10 processing.
C                    R. W. Brode, PES - 12/2/98
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    and to allow flagpole receptors with DEPOS option.
C                    D. Strimaitis, SRC - 2/15/93
C
C        INPUTS:  Pathway (CO) and Keyword
C
C        OUTPUTS: Processing Option Switches
C                 Option Setup Status Switches
C
C        CALLED FROM:   SETUP
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER :: I

C     Variable Initializations
      MODNAM = 'COCARD'

      IF (KEYWRD .EQ. 'STARTING') THEN
         IURB = 0
C        Set Status Switch
         ISTART = .TRUE.
         ICSTAT(1) = ICSTAT(1) + 1
         IF (ICSTAT(1) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLEONE') THEN
C        Set Status Switch
         ICSTAT(2) = ICSTAT(2) + 1
         IF (ICSTAT(2) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF
      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
C        Set Status Switch
         ICSTAT(3) = ICSTAT(3) + 1
         IF (ICSTAT(3) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Titles                                  ---   CALL TITLES
            CALL TITLES
         END IF
      ELSE IF (KEYWRD .EQ. 'MODELOPT') THEN
C        Set Status Switch
         ICSTAT(4) = ICSTAT(4) + 1
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Modeling Options                        ---   CALL MODOPT
            CALL MODOPT
         END IF
      ELSE IF (KEYWRD .EQ. 'AVERTIME') THEN
C        Set Status Switch
         ICSTAT(5) = ICSTAT(5) + 1
         IF (ICSTAT(5) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Averaging Time Options                  ---   CALL AVETIM
            CALL AVETIM
         END IF
      ELSE IF (KEYWRD .EQ. 'POLLUTID') THEN
C        Set Status Switch
         ICSTAT(6) = ICSTAT(6) + 1
         IF (ICSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Pollutant ID Option                     ---   CALL POLLID
            CALL POLLID
         END IF
      ELSE IF (KEYWRD .EQ. 'HALFLIFE' .OR.
     &         KEYWRD .EQ. 'DCAYCOEF') THEN
         IF (KEYWRD .EQ. 'HALFLIFE') THEN
C           Check for Previous DCAYCOEF Keyword in Runstream File
            IF (ICSTAT(8) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
               GO TO 999
            ELSE
C              Set Status Switch and Check for Duplicate Keyword
               ICSTAT(7) = ICSTAT(7) + 1
               IF (ICSTAT(7) .NE. 1) THEN
C                 WRITE Error Message: Repeat Non-repeatable Keyword
                  CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
                  GO TO 999
               END IF
            END IF
         ELSE IF (KEYWRD .EQ. 'DCAYCOEF') THEN
C           Check for Previous HALFLIFE Keyword in Runstream File
            IF (ICSTAT(7) .NE. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','155',KEYWRD)
               GO TO 999
            ELSE
C              Set Status Switch and Check for Duplicate Keyword
               ICSTAT(8) = ICSTAT(8) + 1
               IF (ICSTAT(8) .NE. 1) THEN
C                 WRITE Error Message: Repeat Non-repeatable Keyword
                  CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
                  GO TO 999
               END IF
            END IF
         END IF
C        Check for Keyword Out of Order
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         ELSE IF (ICSTAT(6) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow POLLUTID)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         END IF
C        Process Exponential Decay Option                   ---   CALL EDECAY
         CALL EDECAY
      ELSE IF (KEYWRD .EQ. 'FLAGPOLE') THEN
C        Set Status Switch
         ICSTAT(11) = ICSTAT(11) + 1
         IF (ICSTAT(11) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Flagpole Receptor Height Option         ---   CALL FLAGDF
            CALL FLAGDF
         END IF
      ELSE IF (KEYWRD .EQ. 'RUNORNOT') THEN
C        Set Status Switch
         ICSTAT(12) = ICSTAT(12) + 1
         IF (ICSTAT(12) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Option to Run Model or Not              ---   CALL RUNNOT
            CALL RUNNOT
         END IF
      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'EVENTFIL') THEN
C        Set Status Switch
         ICSTAT(13) = ICSTAT(13) + 1
         IF (ICSTAT(13) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PSDCREDIT) THEN
C              WRITE Warning Message:  PSDCREDIT option cannot be used with EVENT option
               CALL ERRHDL(PATH,MODNAM,'W','147',KEYWRD)
            END IF
C           Process EVENT File Option                       ---   CALL EVNTFL
            CALL EVNTFL
         END IF
      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'SAVEFILE') THEN
C        Set Status Switch
         ICSTAT(14) = ICSTAT(14) + 1
         IF (ICSTAT(14) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Model Re-start Save File Option         ---   CALL SAVEFL
            CALL SAVEFL
         END IF
      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'INITFILE') THEN
C        Set Status Switch
         ICSTAT(15) = ICSTAT(15) + 1
         IF (ICSTAT(15) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Re-start Initialization File Option     ---   CALL INITFL
            CALL INITFL
         END IF
      ELSE IF (.NOT.EVONLY .AND. KEYWRD .EQ. 'MULTYEAR') THEN
C        Set Status Switch
         ICSTAT(16) = ICSTAT(16) + 1
         IF (ICSTAT(16) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Multiple-Year Run Option                ---   CALL MYEAR
            CALL MYEAR
         END IF
      ELSE IF (KEYWRD .EQ. 'ERRORFIL') THEN
C        Set Status Switch
         ICSTAT(17) = ICSTAT(17) + 1
         IF (ICSTAT(17) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL ERRFIL
            CALL ERRFIL
         END IF
      ELSE IF (KEYWRD .EQ. 'GDSEASON') THEN
C        Set Status Switch
         ICSTAT(18) = ICSTAT(18) + 1
         IF (ICSTAT(18) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Seasons for GASDEP Option              ---   CALL GDSEAS
            CALL GDSEAS
         END IF
      ELSE IF (KEYWRD .EQ. 'GASDEPDF') THEN
C        Set Status Switch
         ICSTAT(19) = ICSTAT(19) + 1
         IF (ICSTAT(19) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process GASDEP Defaults Option                  ---   CALL GDDEF
            CALL GDDEF
         END IF
      ELSE IF (KEYWRD .EQ. 'GDLANUSE') THEN
C        Set Status Switch
         ICSTAT(20) = ICSTAT(20) + 1
         IF (ICSTAT(20) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL GDLAND
            CALL GDLAND
         END IF
      ELSE IF (KEYWRD .EQ. 'GASDEPVD') THEN
C        Set Status Switch
         ICSTAT(21) = ICSTAT(21) + 1
         IF (ICSTAT(21) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           User Specified Deposition Velocity Option       ---   CALL GVSUBD
            CALL GVSUBD
         END IF
      ELSE IF (KEYWRD .EQ. 'DEBUGOPT') THEN
C        Set Status Switch
         ICSTAT(22) = ICSTAT(22) + 1
         IF (ICSTAT(22) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
C           Process Error File Option                       ---   CALL DEBOPT
            CALL DEBOPT
         END IF
      ELSE IF (KEYWRD .EQ. 'URBANOPT') THEN
C        Set Status Switch
         ICSTAT(23) = ICSTAT(23) + 1
C        Check for Keyword Out of Order
         IF (ICSTAT(4) .NE. 1) THEN
C           WRITE Error Message: Keyword Out of Order (Must Follow MODELOPT)
            CALL ERRHDL(PATH,MODNAM,'E','140',KEYWRD)
         END IF
C        Process Urban Option                               ---   CALL URBOPT
         CALL URBOPT
      ELSE IF (KEYWRD .EQ. 'OZONEVAL') THEN
C        Set Status Switch
         ICSTAT(24) = ICSTAT(24) + 1
         IF (ICSTAT(24) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PVMRM .OR. OLM) THEN
C              Process O3 Value Option                    ---   CALL O3VAL
               CALL O3VAL
            ELSE
C              Write Error Message:  OZONEVAL specified without PVMRM or OLM
               CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'O3VALUES') THEN
C        Set Status Switch
         ICSTAT(25) = ICSTAT(25) + 1
         IF (PVMRM .OR. OLM) THEN
C           Process O3 Value Option                    ---   CALL O3VALS
            CALL O3VALS
         ELSE
C           Write Error Message:  O3VALUES specified without PVMRM or OLM
            CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
         END IF
      ELSE IF (KEYWRD .EQ. 'OZONEFIL') THEN
C        Set Status Switch
         ICSTAT(26) = ICSTAT(26) + 1
         IF (ICSTAT(26) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PVMRM .OR. OLM) THEN
C              Process O3 File Option                    ---   CALL O3FIL
               CALL O3FIL
            ELSE
C              Write Error Message:  OZONEFIL specified without PVMRM or OLM
               CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'OZONUNIT') THEN 
C        Set Status Switch
         ICSTAT(27) = ICSTAT(27) + 1
         IF (ICSTAT(27) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PVMRM .OR. OLM) THEN
C              Process the OZONUNIT Card                    ---   CALL OZON_UNIT
               CALL OZON_UNIT
            ELSE
C              Write Error Message:  OZONUNIT specified without PVMRM or OLM
               CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'NO2STACK') THEN
C        Set Status Switch
         ICSTAT(28) = ICSTAT(28) + 1
         IF (ICSTAT(28) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PVMRM .OR. OLM) THEN
C              Process NO2Stack Option                    ---   CALL NO2STK
               CALL NO2STK
            ELSE
C              Write Error Message:  NO2STACK specified without PVMRM or OLM
               CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'NO2EQUIL') THEN
C        Set Status Switch
         ICSTAT(29) = ICSTAT(29) + 1
         IF (ICSTAT(29) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
         ELSE
            IF (PVMRM .OR. OLM) THEN
C              Process NO2Equil Option                    ---   CALL NO2EQ
               CALL NO2EQ
            ELSE
C              Write Error Message:  NO2EQUIL specified without PVMRM or OLM
               CALL ERRHDL(PATH,MODNAM,'E','142',KEYWRD)
            END IF
         END IF
      ELSE IF (KEYWRD .EQ. 'FINISHED') THEN
C        Set Status Switch
         IFINIS = .TRUE.
C        Set Status Switch
         ICSTAT(50) = ICSTAT(50) + 1
         IF (ICSTAT(50) .NE. 1) THEN
C           WRITE Error Message: Repeat Non-repeatable Keyword
            CALL ERRHDL(PATH,MODNAM,'E','135',KEYWRD)
            GO TO 999
         END IF

C        Check for Missing Mandatory Keywords
         IF (ICSTAT(1) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','STARTING')
         END IF
         IF (ICSTAT(2) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','TITLEONE')
         END IF
         IF (ICSTAT(4) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','MODELOPT')
         END IF
         IF (ICSTAT(5) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','AVERTIME')
         END IF
         IF (ICSTAT(6) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','POLLUTID')
         END IF
         IF (ICSTAT(12) .EQ. 0) THEN
            CALL ERRHDL(PATH,MODNAM,'E','130','RUNORNOT')
         END IF

         IF (OLM .OR. PVMRM) THEN
C ---       Check for background Ozone options for OLM/PVMRM
            IF (ICSTAT(24).EQ.0 .AND. ICSTAT(25).EQ.0 .AND. 
     &                                ICSTAT(26).EQ.0) THEN
C              Write Error Message:  Ozone value or data file needed
               IF (OLM) THEN
                  DUMMY = '  OLM  '
               ELSE IF (PVMRM) THEN
                  DUMMY = ' PVMRM '
               END IF
               CALL ERRHDL(PATH,MODNAM,'E','283',DUMMY)
            ELSE IF (ICSTAT(24).GT.0 .AND. ICSTAT(25).GT.0) THEN
C              Write Warning Message:  O3VALUES keyword will be used
C              instead of OZONEVAL keyword
               IF (OLM) THEN
                  DUMMY = 'for OLM   '
               ELSE IF (PVMRM) THEN
                  DUMMY = 'for PVMRM '
               END IF
               CALL ERRHDL(PATH,MODNAM,'W','148',DUMMY)
            END IF
            IF ((PVMRM .OR. OLM) .AND. ICSTAT(28).EQ.0) THEN
C              No NO2STACK card specified for PVMRM or OLM options.  
C              Reinitialize ANO2_RATIO array to -9.0 to track whether 
C              NO2/NOx ratios are applied on the SO Pathway with 
C              NO2RATIO card for each source.
               ANO2_RATIO(:) = -9.0D0
            END IF
C ---       Check for OZONUNIT keyword without O3VALUES keyword
            IF (ICSTAT(25) .EQ. 0 .AND. ICSTAT(27) .GT. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'E','193','CO O3VALUES')
            END IF
         END IF

C        OPEN Restart Save and Initialization Files
         IF (RSTSAV) THEN
            DUMMY = 'SAVEFILE'
            OPEN(UNIT=IDPUNT,ERR=99,FILE=SAVFIL,FORM='UNFORMATTED',
     &           IOSTAT=IOERRN,STATUS='REPLACE')
C           Close SAVEFILE since it is re-opened in RSDUMP
            CLOSE (IDPUNT)
            IF (SAVFL2 .NE. SAVFIL) THEN
               OPEN(UNIT=IDPUN2,ERR=99,FILE=SAVFL2,FORM='UNFORMATTED',
     &              IOSTAT=IOERRN,STATUS='REPLACE')
C              Close SAVEFILE since it is re-opened in RSDUMP
               CLOSE (IDPUN2)
            END IF
         END IF
         IF (RSTINP) THEN
            IF (RSTSAV) THEN
C ---          First check for filename conflicts with SAVEFILEs
               IF (INIFIL .EQ. SAVFIL .OR. INIFIL .EQ. SAVFL2) THEN
C ---             The INITFILE name matches a SAVEFILE name;
C                 issue error message
                  CALL ERRHDL(PATH,MODNAM,'E','590','       ')
               ELSE
C ---             No filename conflict, open INITFILE
                  DUMMY = 'INITFILE'
                  OPEN(UNIT=IRSUNT,ERR=99,FILE=INIFIL,
     &                 FORM='UNFORMATTED',IOSTAT=IOERRN,STATUS='OLD')
               END IF
            ELSE
C ---          No SAVEFILEs, so open INITFILE
               DUMMY = 'INITFILE'
               OPEN(UNIT=IRSUNT,ERR=99,FILE=INIFIL,FORM='UNFORMATTED',
     &              IOSTAT=IOERRN,STATUS='OLD')
            END IF
         END IF

C        Check Averaging Periods Selected for SCREEN Mode Option
         IF (SCREEN) THEN
            IF (NUMAVE .GT. 1) THEN
C              WRITE Error Message:  Too Many Averaging Periods Selected
               CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
            ELSE IF (KAVE(1) .NE. 1) THEN
C              WRITE Error Message:  Invalid Averaging Period Selected
               CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
            END IF
            IF (PERIOD) THEN
C              WRITE Error Message:  Too Many Averaging Periods Selected
               CALL ERRHDL(PATH,MODNAM,'E','295',' 1h Only')
            END IF
         END IF

C ---    Check for non-DFAULT gas deposition options
         IF (DFAULT .AND. ICSTAT(18) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GDSEASON')
         ELSE IF (ICSTAT(18) .GT. 0) THEN
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF
         IF (DFAULT .AND. ICSTAT(19) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GASDEPDF')
         ELSE IF (ICSTAT(19) .GT. 0) THEN
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF
         IF (DFAULT .AND. ICSTAT(20) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GDLANUSE')
         ELSE IF (ICSTAT(20) .GT. 0) THEN
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF
         IF (DFAULT .AND. ICSTAT(21) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ DFAULT Option
            CALL ERRHDL(PATH,MODNAM,'E','196','GASDEPVD')
         ELSE IF (ICSTAT(21) .GT. 0) THEN
C           Set flag for use of non-DEFAULT option
            L_NonDFAULT = .TRUE.
         END IF

C ---    Check for incompatibilities with user-specified deposition velocity
         IF (LUSERVD .AND. (DEPOS .OR. WDEP .OR. WDPLETE)) THEN
C           Write Error Message: Wet deposition output incompatible with GASDEPVD option
            CALL ERRHDL(PATH,MODNAM,'E','243','GASDEPVD')
         END IF

C ---    Check for incompatible gas deposition inputs with GASDEPVD option for
C        user-specified gas dry deposition velocity
         IF (LUSERVD .AND. ICSTAT(18) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
            CALL ERRHDL(PATH,MODNAM,'E','195','GDSEASON')
         END IF
         IF (LUSERVD .AND. ICSTAT(19) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
            CALL ERRHDL(PATH,MODNAM,'E','195','GASDEPDF')
         END IF
         IF (LUSERVD .AND. ICSTAT(20) .GT. 0) THEN
C           Write Error Message:  Gas Dry Deposition Option w/ user-specified GASDEPVD
            CALL ERRHDL(PATH,MODNAM,'E','195','GDLANUSE')
         END IF

C        Generate MODOPS Character Array to Summarize Modeling Options
         IF (DFAULT) THEN
            MODOPS(1) = 'RegDFAULT'
         ELSE IF (L_NonDFAULT) THEN
            MODOPS(1) = 'NonDFAULT'
         ELSE
            MODOPS(1) = '         '
         END IF
         
         IF (CONC) THEN
            MODOPS(2) = 'CONC'
         END IF
         IF (DEPOS) THEN
            MODOPS(3) = 'DEPOS'
         END IF
         IF (DDEP) THEN
            MODOPS(4) = 'DDEP'
         END IF
         IF (WDEP) THEN
            MODOPS(5) = 'WDEP'
         END IF
         
         IF (FLATSRCS) THEN
            MODOPS(6) = 'FLAT and'
            MODOPS(7) = 'ELEV'
         ELSE IF (FLAT) THEN
            MODOPS(6) = 'FLAT'
         ELSE
            MODOPS(7) = 'ELEV'
         END IF
         
         IF (FLGPOL) MODOPS(8)  = 'FLGPOL'
         IF (NOSTD)  MODOPS(9)  = 'NOSTD'
         IF (NOCHKD) THEN
            MODOPS(10) = 'NOCHKD'
         ELSE IF (L_WARNCHKD) THEN
            MODOPS(10) = 'WARNCHKD'
         END IF
         
         IF (FASTALL) THEN
            MODOPS(11) = 'FASTALL'
         ELSE IF (FASTAREA) THEN
            MODOPS(11) = 'FASTAREA'
         ELSE IF (NOWARN) THEN
            MODOPS(11) = 'NOWARN'
         END IF
         
         IF (SCREEN) MODOPS(12) = 'SCREEN'
         IF (MULTYR) MODOPS(13) = 'MULTYR'
         
         IF (ARDPLETE) THEN
            MODOPS(14) = 'AREADPLT'
         ELSE IF (ROMBERG) THEN
            MODOPS(14) = 'ROMBERG'
         ELSE IF (DDPLETE) THEN
            MODOPS(14) = 'DRYDPLT'
         ELSE IF (.NOT.DDPLETE) THEN
            MODOPS(14) = 'NODRYDPLT'
         END IF
         
         IF (WDPLETE) THEN
            MODOPS(15) = 'WETDPLT'
         ELSE IF (.NOT.WDPLETE) THEN
            MODOPS(15) = 'NOWETDPLT'
         END IF
         
         IF (SCIM) MODOPS(16) = 'SCIM'
         
         IF (PVMRM) THEN
            MODOPS(17) = 'PVMRM'
         ELSE IF (OLM) THEN
            MODOPS(17) = 'OLM'
         END IF
         
         IF (PSDCREDIT) THEN
            MODOPS(18) = 'PSDCREDIT'
         ENDIF
         
C---     Add label for non-DFAULT BETA Option
         IF (.NOT. DFAULT .AND. BETA) THEN
            MODOPS(19) = 'BETA'
         END IF

C---     Add label for NoUrbTran Non-regulatory Option
         IF (.NOT. L_UrbanTransition) THEN
            MODOPS(20) = 'NoUrbTran'
         END IF

         IF (SCIM .AND. NUMAVE.GT.0) THEN
C           Write Error Message:  Cannot use SCIM with short term averages
            CALL ERRHDL(PATH,MODNAM,'E','154','ST AVES')
         END IF
         IF (SCIM .AND. PERIOD) THEN
C           Write Error Message:  Cannot use SCIM with PERIOD average
            CALL ERRHDL(PATH,MODNAM,'E','154','PERIOD')
         END IF
         IF (SCIM .AND. DEPOS) THEN
C           Write Warning Message:  Ignore DEPOS when using SCIM
            DEPOS = .FALSE.
            NUMTYP = NUMTYP - 1
            CALL ERRHDL(PATH,MODNAM,'W','156',' DEPOS ')
         END IF

C        Adjust output label for ANNUAL average deposition fluxes
         IF (ANNUAL) THEN
            DO ITYP = 1, NUMTYP
               IF (.NOT.CONC .OR. ITYP.GT.1) THEN
                  PERLBL(ITYP) = 'GRAMS/M**2/YR'
               END IF
            END DO
         END IF

         IF (L_O3VALUES) THEN
C ---       Check for correct number of temporally varying ozone concentrations
C           for the O3VALUES keyword.
            IF (IO3SET .LT. NO3F) THEN
C              WRITE Error Message: Not Enough O3VALUES values
               WRITE(DUMMY,'(''NO3F='',I6)') NO3F
               CALL ERRHDL(PATH,MODNAM,'E','261',DUMMY)
            END IF
C ---       Check for user-specified ozone units; apply default if needed
            IF (ICSTAT(27) .NE. 1) THEN
               OzoneUnits = 'PPB'
            END IF
         END IF

C ---    Check for PM25 processing
         IF ((POLLUT .EQ. 'PM25'  .OR. POLLUT .EQ. 'PM-2.5' .OR.
     &        POLLUT .EQ. 'PM-25' .OR. POLLUT .EQ. 'PM2.5')) THEN
            IF(.NOT.NOCHKD .AND. .NOT.L_WARNCHKD .AND. .NOT.EVONLY) THEN
C ---          Set logical flag for PM25 processing, averaged across years
               PM25AVE = .TRUE.
C ---          Now check for appropriate averaging periods for PM2.5
               IF (NUMAVE.GT.1 .OR. (NUMAVE.EQ.1 .AND. 
     &                              KAVE(1).NE.24)) THEN
C ---             Write Error Message: Short Term average must be 24-hr only
                  DO I = 1, NUMAVE
                     IF (KAVE(I) .NE. 24) THEN
                        WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                        CALL ERRHDL(PATH,MODNAM,'E','363',DUMMY)
                     END IF
                  END DO
                  PM25AVE = .FALSE.
               END IF
               IF (PERIOD) THEN
C ---             Write Error Message: Long term average must be ANNUAL
                  CALL ERRHDL(PATH,MODNAM,'E','363','PERIOD Ave')
                  PM25AVE = .FALSE.
               END IF
            ELSE IF (.NOT.SCREEN .AND. .NOT.EVONLY) THEN
C ---          Set to false for NOCHKD or WARNCHKD options, without the SCREEN or 
C              EVONLY options, and issue warning message
               IF (NOCHKD) THEN
                  DUMMY = 'NOCHKD'
               ELSE IF (L_WARNCHKD) THEN
                  DUMMY = 'WARNCHKD'
               END IF
               CALL ERRHDL(PATH,MODNAM,'W','363',DUMMY)
               PM25AVE = .FALSE.
            ELSE IF (SCREEN .OR. EVONLY) THEN
C ---          Set PM25AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
               PM25AVE = .FALSE.
            END IF
         END IF

C ---    Check for NO2 1-hour processing
         IF (POLLUT .EQ. 'NO2' .AND. .NOT.NOCHKD .AND. 
     &                           .NOT.L_WARNCHKD .AND. .NOT.EVONLY) THEN
C ---       Check for averaging periods to determine if multi-year
C           processing of maximum daily 1-hour averages is being done
            IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. .NOT.PERIOD 
     &                                         .AND. .NOT.ANNUAL) THEN
C ---          Set logical flag for 1-hr NO2 processing, averaged across years,
C              without PERIOD or ANNUAL averages
               NO2AVE = .TRUE.
            ELSE IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. PERIOD 
     &                                              .AND. MULTYR) THEN
C ---          Set logical flag for 1-hr NO2 processing, averaged across years,
C              using MULTYEAR option to address PERIOD averages
               NO2AVE = .TRUE.
            ELSE IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. 
     &                                       (PERIOD .OR. ANNUAL) .AND.
     &                                               .NOT.MULTYR) THEN
C ---          Write Warning Message: PERIOD averages should not be
C              processed with 1-hour averages for NO2 unless only 1 year
C              of met data is being processed or if the MULTYEAR option
C              is specified.
               CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
C ---          Allow processing to continue, but long-term results
C              may be wrong.
               NO2AVE = .TRUE.
            ELSE IF (NUMAVE.GT.1 .OR. 
     &              (NUMAVE.EQ.1 .AND. KAVE(1).NE.1) .AND.
     &                                       (PERIOD .OR. ANNUAL) .AND.
     &                                               .NOT.MULTYR) THEN
C ---          Write Warning Message: PERIOD averages should not be
C              processed with 1-hour averages for NO2 unless only 1 year
C              of met data is being processed or if the MULTYEAR option
C              is specified.
               CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
C ---          Write Warning Message: Non-standard short term average for NO2
               DO I = 1, NUMAVE
                  IF (KAVE(I) .NE. 1) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
                  END IF
               END DO
               NO2AVE = .FALSE.
            ELSE IF (NUMAVE.GT.1 .OR. 
     &              (NUMAVE.EQ.1 .AND. KAVE(1).NE.1) ) THEN
C ---          Write Warning Message: Non-standard short term average for NO2
C ---          Write Warning Message: Short Term average should be 1-hr only for NO2
               DO I = 1, NUMAVE
                  IF (KAVE(I) .NE. 1) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
                  END IF
               END DO
               NO2AVE = .FALSE.
            ELSE
C ---          Period or Annual average only, set NO2AVE = .F. but allow
C              processing
               NO2AVE = .FALSE.
            END IF
         ELSE IF (POLLUT .EQ. 'NO2' .AND. .NOT.SCREEN .AND. 
     &                                    .NOT.EVONLY) THEN
C ---       Set to false for NOCHKD or WARNCHKD options, without the SCREEN or 
C           EVONLY options, and issue warning message
            IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. 
     &                           ((.NOT.PERIOD .AND. .NOT.ANNUAL) .OR.
     &                                      (PERIOD .AND. MULTYR) .OR.
     &                  ((PERIOD .OR. ANNUAL) .AND. .NOT.MULTYR)) )THEN
               IF (NOCHKD) THEN
                  DUMMY = 'NOCHKD'
               ELSE IF (L_WARNCHKD) THEN
                  DUMMY = 'WARNCHKD'
               END IF
               CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               NO2AVE = .FALSE.
            END IF
         ELSE IF (POLLUT .EQ. 'NO2' .AND. (SCREEN .OR. EVONLY)) THEN
C ---       Set NO2AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
            NO2AVE = .FALSE.
         END IF

C ---    Check for SO2 1-hour processing
         IF (POLLUT .EQ. 'SO2' .AND. .NOT.NOCHKD .AND. 
     &                           .NOT.L_WARNCHKD .AND. .NOT.EVONLY) THEN
C ---       Check for averaging periods to determine if multi-year
C           processing of maximum daily 1-hour averages is being done
            IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. .NOT.PERIOD 
     &                                         .AND. .NOT.ANNUAL) THEN
C ---          Set logical flag for 1-hr SO2 processing, averaged across years
               SO2AVE = .TRUE.
            ELSE IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. PERIOD 
     &                                              .AND. MULTYR) THEN
C ---          Set logical flag for 1-hr SO2 processing, averaged across years,
C              using MULTYEAR option to address PERIOD averages
               SO2AVE = .TRUE.
            ELSE IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. 
     &                                       (PERIOD .OR. ANNUAL) .AND.
     &                                               .NOT.MULTYR) THEN
C ---          Write Warning Message: PERIOD averages should not be
C              processed with 1-hour averages for SO2 unless only 1 year
C              of met data is being processed or if the MULTYEAR option
C              is specified.
               CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
C ---          Allow processing to continue, but long-term results
C              may be wrong.
               SO2AVE = .TRUE.
            ELSE IF (NUMAVE.GT.1 .AND. MINVAL(KAVE).EQ.1 .AND. 
     &                                       (PERIOD .OR. ANNUAL) .AND.
     &                                               .NOT.MULTYR) THEN
C ---          Write Warning Message: PERIOD averages should not be
C              processed with 1-hour averages for SO2 unless only 1 year
C              of met data is being processed or if the MULTYEAR option
C              is specified.
               CALL ERRHDL(PATH,MODNAM,'W','361','MULTYEAR Opt')
C ---          Write Warning Message: Non-standard short term average for SO2
               DO I = 1, NUMAVE
                  IF (KAVE(I) .NE. 1) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
                  END IF
               END DO
C ---          Allow processing to continue, but without special processing
C              of 1-hr values averaged across years
               SO2AVE = .FALSE.
            ELSE IF (NUMAVE.GT.1 .AND. MINVAL(KAVE).EQ.1) THEN
C ---          Write Warning Message: Non-standard short term average for SO2
               DO I = 1, NUMAVE
                  IF (KAVE(I) .NE. 1) THEN
                     WRITE(DUMMY,'(I3,''-hr Ave'')') KAVE(I)
                     CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
                  END IF
               END DO
C ---          Allow processing to continue, but without special processing
C              of 1-hr values averaged across years
               SO2AVE = .FALSE.
            ELSE
C ---          Period or Annual average only, set SO2AVE = .F. but allow
C              processing
               SO2AVE = .FALSE.
            END IF
         ELSE IF (POLLUT .EQ. 'SO2' .AND. .NOT.SCREEN .AND. 
     &                                    .NOT.EVONLY) THEN
C ---       Set to false for NOCHKD or WARNCHKD options, without the SCREEN or 
C           EVONLY options, and issue warning message
            IF (NUMAVE.EQ.1 .AND. KAVE(1).EQ.1 .AND. 
     &                           ((.NOT.PERIOD .AND. .NOT.ANNUAL) .OR.
     &                                      (PERIOD .AND. MULTYR) .OR.
     &                   ((PERIOD .OR. ANNUAL) .AND. .NOT.MULTYR)) )THEN
               IF (NOCHKD) THEN
                  DUMMY = 'NOCHKD'
               ELSE IF (L_WARNCHKD) THEN
                  DUMMY = 'WARNCHKD'
               END IF
               CALL ERRHDL(PATH,MODNAM,'W','362',DUMMY)
               SO2AVE = .FALSE.
            END IF
         ELSE IF (POLLUT .EQ. 'SO2' .AND. (SCREEN .OR. EVONLY)) THEN
C ---       Set SO2AVE to .FALSE. for SCREEN or EVONLY options, without any warnings
            SO2AVE = .FALSE.
         END IF

C ---    Check for pollutant ID = 'NO2' for PVMRM and OLM options
         IF ((PVMRM .OR. OLM) .AND. POLLUT .NE. 'NO2') THEN
C           Write Error Message:  Pollutant ID doesn't match option
            CALL ERRHDL(PATH,MODNAM,'E','284',' NO2 ')
         END IF

C ---    Check for PM25, NO2, or SO2 processing based on ranked values
C        averaged across years, and adjust PLOTFILE format accordingly
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
           PLTFRM = '(3(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,A8,2X,
     &10(F13.5,2X,I8.8,2X:))'
         END IF
       
         GO TO 1000

C        WRITE Error Message for Error Opening File
 99      CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)
         IF (DUMMY .EQ. 'SAVEFILE') THEN
C           Reset Logical Flag for SAVEFILE Option Due to Error Opening File
            RSTSAV = .FALSE.
         ELSE IF (DUMMY .EQ. 'INITFILE') THEN
C           Reset Logical Flag for INITFILE Option Due to Error Opening File
            RSTINP = .FALSE.
         END IF

 1000    CONTINUE

      ELSE
C        Write Error Message: Invalid Keyword for This Pathway
         CALL ERRHDL(PATH,MODNAM,'E','110',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE TITLES
C***********************************************************************
C                 TITLES Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Title Information From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Title Strings for Model Outputs
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'TITLES'

      IF (KEYWRD .EQ. 'TITLEONE') THEN
         TITLE1 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),
     &                                         (LOCE(2)+2+ILEN_FLD-1)))
         IF (TITLE1 .EQ. ' ') THEN
C*          Write Error Message: Missing Parameter Title
            CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
         END IF

      ELSE IF (KEYWRD .EQ. 'TITLETWO') THEN
         TITLE2 = RUNST1(LOCE(2)+2:MIN(LEN_TRIM(RUNST1),
     &                                         (LOCE(2)+2+ILEN_FLD-1)))
         IF (TITLE2 .EQ. ' ') THEN
C*          Write Warning Message
            CALL ERRHDL(PATH,MODNAM,'W','200',KEYWRD)
         END IF

      END IF

      RETURN
      END

      SUBROUTINE MODOPT
C***********************************************************************
C                 MODOPT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Modeling Options From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To incorporate additional options and adjust 
C                    the handling of options that conflict with the
C                    regulatory DFAULT option.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
C
C        MODIFIED:   To incorporate undocumented options to turn off
C                    depletion, which is now the default.
C                    R. W. Brode, MACTEC/PES - 10/26/2004
C
C        MODIFIED:   To allow for calculating CONC/DEPOS/DDEP/WDEP in
C                    a single model run.
C                    R. W. Brode, PES - 4/17/95
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DEPLETE parameter for plume depletion option
C                    D. Strimaitis, SRC - 2/15/93
C
C        MODIFIED:   To Output Warning Message '206' For Overriding
C                    Non-DEFAULT Option - 9/29/92
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Modeling Option Logical Switch Settings
C
C        ERROR HANDLING:   Checks for Too Few or Too Many Option Keywords;
C                          Checks for Invalid Option Keywords;
C                          Checks for Conflicting or Missing Option Keywords
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      CHARACTER KOPT*9

C     Variable Initializations - Initialize All Logical Switches to FALSE
      MODNAM = 'MODOPT'

C     Check for Too Few or Too Many Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE IF (IFC .GT. 14) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'W','202',KEYWRD)
      END IF

C     First Check for Presence of DFAULT Switch
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'DFAULT' .OR. KOPT .EQ. 'DEFAULT') THEN
            DFAULT    = .TRUE.
            ELEV      = .TRUE.
            FLAT      = .FALSE.
            FLATSRCS  = .FALSE.
            MSGPRO    = .TRUE.
            NOSTD     = .FALSE.
            NOCHKD    = .FALSE.
            SCREEN    = .FALSE.
            SCIM      = .FALSE.
            PVMRM     = .FALSE.
            PSDCREDIT = .FALSE.
            OLM       = .FALSE.
            BETA      = .FALSE.
            FASTAREA  = .FALSE.
            FASTALL   = .FALSE.
            L_EFFSIGY = .FALSE.
            L_NonDFAULT = .FALSE.
            L_UrbanTransition = .TRUE.

            EXIT
         END IF
      END DO

C     Next check for presence of both FLAT and ELEV if NOT.DFAULT
      IF (.NOT. DFAULT) THEN
C        First look for FLAT
         DO I = 3, IFC
            KOPT = FIELD(I)
            IF (KOPT .EQ. 'FLAT') THEN
               FLAT = .TRUE.
               ELEV = .FALSE.
               EXIT
            END IF
         END DO
C        If FLAT, next look for ELEV, indicating both FLAT and
C        ELEV sources in the same run (FLATSRCS)
         IF (FLAT) THEN
            DO I = 3, IFC
               KOPT = FIELD(I)
               IF (KOPT .EQ. 'ELEV') THEN
                  ELEV     = .TRUE.
                  FLATSRCS = .TRUE.
                  EXIT
               END IF
            END DO
         END IF
      ELSE
C        Look for FLAT with DFAULT
         DO I = 3, IFC
            KOPT = FIELD(I)
            IF (KOPT .EQ. 'FLAT') THEN
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         END DO
      END IF
                  
C     Next Check for Presence of BETA Switch
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'BETA' .AND. .NOT.DFAULT) THEN
            BETA = .TRUE.
         END IF
      END DO

      NUMTYP = 0
C     Loop Through Fields Again Setting All Swithes
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'DFAULT' .OR. KOPT .EQ. 'DEFAULT') THEN
            DFAULT = .TRUE.
         ELSE IF (KOPT .EQ. 'CONC') THEN
            IF (.NOT. CONC) THEN
               CONC   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
         ELSE IF (KOPT .EQ. 'DEPOS') THEN
            IF (.NOT. DEPOS) THEN
               DEPOS  = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
         ELSE IF (KOPT .EQ. 'DDEP') THEN
            IF (.NOT. DDEP) THEN
               DDEP   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
         ELSE IF (KOPT .EQ. 'WDEP') THEN
            IF (.NOT. WDEP) THEN
               WDEP   = .TRUE.
               NUMTYP = NUMTYP + 1
            END IF
         ELSE IF (KOPT .EQ. 'FLAT' .OR. KOPT .EQ. 'ELEV') THEN
            CYCLE
         ELSE IF (KOPT .EQ. 'DRYDPLT' .AND. .NOT.NODRYDPLT) THEN
            DDPLETE = .TRUE.
            DRYDPLT = .TRUE.
         ELSE IF (KOPT .EQ. 'DRYDPLT' .AND. NODRYDPLT) THEN
C ---       Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE IF (KOPT .EQ. 'NODRYDPLT' .AND. .NOT.DRYDPLT) THEN
C           Dry depletion is now standard - include "option" to override it
            DDPLETE = .FALSE.
C           Set separate logical for user-specified option to ensure that 
C           it is reflected in the page header
            NODRYDPLT = .TRUE.
         ELSE IF (KOPT .EQ. 'NODRYDPLT' .AND. DRYDPLT) THEN
C ---       Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE IF (KOPT .EQ. 'ROMBERG') THEN
            ROMBERG = .TRUE.
            DDPLETE = .TRUE.
         ELSE IF (KOPT .EQ. 'AREADPLT') THEN
            IF (.NOT. DFAULT) THEN
               ARDPLETE = .TRUE.
               DDPLETE  = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'WETDPLT' .AND. .NOT.NOWETDPLT) THEN
            WDPLETE = .TRUE.
            WETDPLT = .TRUE.
         ELSE IF (KOPT .EQ. 'WETDPLT' .AND. NOWETDPLT) THEN
C ---       Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE IF (KOPT .EQ. 'NOWETDPLT' .AND. .NOT.WETDPLT) THEN
C           Wet depletion is now standard - include "option" to override it
            WDPLETE = .FALSE.
C           Set separate logical for user-specified option to ensure that 
C           it is reflected in the page header
            NOWETDPLT = .TRUE.
         ELSE IF (KOPT .EQ. 'NOWETDPLT' .AND. WETDPLT) THEN
C ---       Write Error Message        ! Conflicting options specified
            CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
         ELSE IF (KOPT .EQ. 'NOSTD') THEN
            IF (.NOT. DFAULT) THEN
               NOSTD = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOWARN') THEN
            NOWARN = .TRUE.
         ELSE IF (KOPT .EQ. 'NOCHKD') THEN
            IF (.NOT. DFAULT .AND. .NOT.L_WARNCHKD) THEN
               NOCHKD = .TRUE.
            ELSE IF (.NOT.DFAULT .AND. L_WARNCHKD) THEN
C ---          Write Error Message        ! Conflicting options specified
               CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'WARNCHKD') THEN
            IF (.NOT. NOCHKD) THEN
               L_WARNCHKD = .TRUE.
            ELSE
C ---          Write Error Message        ! Conflicting options specified
               CALL ERRHDL(PATH,MODNAM,'E','149',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'SCREEN') THEN
            IF (.NOT. DFAULT) THEN
               SCREEN = .TRUE.
C              Set NOCHKD option on for SCREEN mode
               NOCHKD = .TRUE.
            ELSE
C              WRITE Warning Message     ! Non-DEFAULT Option Overridden
               CALL ERRHDL(PATH,MODNAM,'W','206',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'SCIM') THEN
            IF (.NOT. DFAULT) THEN
               SCIM = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'TOXICS') THEN
C ---       WRITE Warning Message        ! TOXICS option is obsolete
C           If this run includes area or openpit sources, refer to 
C           FASTAREA option and set the logical flag
            IF (NAREA .GT. 0 .OR. NPIT .GT. 0) THEN
               CALL ERRHDL(PATH,MODNAM,'W','198','FASTAREA')
            ELSE
               CALL ERRHDL(PATH,MODNAM,'W','198','        ')
            END IF
            IF (.NOT. DFAULT) THEN
               IF (NAREA .GT. 0 .OR. NPIT .GT. 0) THEN
C ---             Assign FASTAREA option to TRUE for consistency with
C                 area source optimizations under obsolete TOXICS option
                  FASTAREA = .TRUE.
               END IF
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'PVMRM') THEN
            IF (.NOT. DFAULT) THEN
               PVMRM = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'OLM') THEN
            IF (.NOT. DFAULT) THEN
               OLM = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'PSDCREDIT') THEN                           ! jop 093006
            IF (BETA .AND. .NOT. DFAULT) THEN
               PSDCREDIT = .TRUE.
            ELSE IF (.NOT. BETA .AND. .NOT. DFAULT) THEN
C              WRITE Error Message     ! BETA Option Required for PSDCREDIT
               CALL ERRHDL(PATH,MODNAM,'E','199',KOPT)
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'BETA') THEN
            IF (.NOT. DFAULT) THEN
               BETA = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'FASTAREA') THEN
            IF (FASTALL) THEN
C              Issue warning message since FASTALL implies FASTAREA
               CALL ERRHDL(PATH,MODNAM,'W','192','        ')
            END IF
            IF (.NOT. DFAULT) THEN
C              Set logical flag for FASTAREA for optimized area source;
C              equivalent to optimizations associated with obsolete TOXICS option
               FASTAREA = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'FASTALL') THEN
            IF (FASTAREA) THEN
C              Issue warning message since FASTALL implies FASTAREA
               CALL ERRHDL(PATH,MODNAM,'W','192','        ')
            END IF
            IF (.NOT. DFAULT) THEN
C ---          Set logical flag for FASTAREA for optimized area source;
C              equivalent to optimizations associated with obsolete TOXICS option.
               FASTAREA = .TRUE.
C ---          Also set L_EFFSIGY option flag for optimized meander option for
C              point and volume sources.
               FASTALL   = .TRUE.
               L_EFFSIGY = .TRUE.
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE IF (KOPT .EQ. 'NOURBTRAN') THEN
            IF (.NOT. DFAULT) THEN
C ---          Non-regulatory option to ignore transition from nighttime urban
C              enhanced boundary layer to daytime convective boundary
C ---          Set logical switch to account for urban transition to .F. and 
C              issue warning message
               L_UrbanTransition = .FALSE.
C              WRITE Warning Message   
               CALL ERRHDL(PATH,MODNAM,'W','151','Keyword     ')
            ELSE
C              WRITE Error Message     ! Non-DEFAULT Option Conflict
               CALL ERRHDL(PATH,MODNAM,'E','204',KOPT)
            END IF
         ELSE
C           WRITE Error Message     ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KOPT)
         END IF
      END DO

      IF (OLM .AND. PVMRM) THEN
C        WRITE Error Message       ! Can't specify PVMRM and OLM
         CALL ERRHDL(PATH,MODNAM,'E','141','        ')
      END IF

      IF (PSDCREDIT .AND. .NOT. PVMRM) THEN
C        WRITE Error Message       ! Can't specify PSDCREDIT without PVMRM
         CALL ERRHDL(PATH,MODNAM,'E','143','PSDCREDIT')
      END IF
      
      IF (PSDCREDIT .AND. EVONLY) THEN
C        WRITE Error Message       ! Can't use PSDCREDIT with EVONLY Processing
         CALL ERRHDL(PATH,MODNAM,'E','147',' EVENTS ')
      END IF

C --- Check for Non-DFAULT options used without DFAULT; this will be used to
C     set the option label in the file headers.
      IF (.NOT. DFAULT .AND. 
     &    (FLAT .OR. FLATSRCS .OR. ARDPLETE .OR. NOSTD .OR. NOCHKD .OR. 
     &     SCREEN .OR. SCIM .OR. PVMRM .OR. OLM .OR. PSDCREDIT .OR. 
     &     BETA .OR. FASTAREA .OR. FASTALL .OR. 
     &                                    .NOT.L_UrbanTransition)) THEN
     
         L_NonDFAULT = .TRUE.
      END IF
      
C     Setup Label Array for Concentration and Depositions
      IF (NUMTYP .GT. NTYP) THEN
C        WRITE Error Message: Number of output types exceeds maximum
         WRITE(DUMMY,'(I4)') NTYP
         CALL ERRHDL(PATH,MODNAM,'E','280',DUMMY)
      ELSE IF (NUMTYP .EQ. 0) THEN
C        WRITE Warning Message: No Output Types Selected, Assume CONC Only
         CALL ERRHDL(PATH,MODNAM,'W','205','CONC')
         NUMTYP = 1
         ITYP   = 1
         CONC   = .TRUE.
         CHIDEP(1,ITYP) = 'AVER'
         CHIDEP(2,ITYP) = 'AGE '
         CHIDEP(3,ITYP) = 'CONC'
         CHIDEP(4,ITYP) = 'ENTR'
         CHIDEP(5,ITYP) = 'ATIO'
         CHIDEP(6,ITYP) = 'N   '
         EMIFAC(ITYP) = 1.0D06
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'MICROGRAMS/M**3'
         PERLBL(ITYP) = 'MICROGRAMS/M**3'
         OUTTYP(ITYP) = 'CONC'
      ELSE IF (CONC) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = 'AVER'
         CHIDEP(2,ITYP) = 'AGE '
         CHIDEP(3,ITYP) = 'CONC'
         CHIDEP(4,ITYP) = 'ENTR'
         CHIDEP(5,ITYP) = 'ATIO'
         CHIDEP(6,ITYP) = 'N   '
         EMIFAC(ITYP) = 1.0D06
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'MICROGRAMS/M**3'
         PERLBL(ITYP) = 'MICROGRAMS/M**3'
         OUTTYP(ITYP) = 'CONC'
         IF (DEPOS) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '  TO'
            CHIDEP(2,ITYP) = 'TAL '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'DEPOS'
            IF (DDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'DRY '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'DDEP'
               IF (WDEP) THEN
                  ITYP = 4
                  CHIDEP(1,ITYP) = '    '
                  CHIDEP(2,ITYP) = 'WET '
                  CHIDEP(3,ITYP) = 'DEPO'
                  CHIDEP(4,ITYP) = 'SITI'
                  CHIDEP(5,ITYP) = 'ON  '
                  CHIDEP(6,ITYP) = '    '
                  EMIFAC(ITYP) = 3600.0D0
                  EMILBL(ITYP) = 'GRAMS/SEC'
                  OUTLBL(ITYP) = 'GRAMS/M**2'
                  PERLBL(ITYP) = 'GRAMS/M**2'
                  OUTTYP(ITYP) = 'WDEP'
               END IF
            ELSE IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'WDEP'
            END IF
         ELSE IF (DDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'DDEP'
            IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'WDEP'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (DEPOS) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '  TO'
         CHIDEP(2,ITYP) = 'TAL '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'DEPOS'
         IF (DDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'DRY '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'DDEP'
            IF (WDEP) THEN
               ITYP = 3
               CHIDEP(1,ITYP) = '    '
               CHIDEP(2,ITYP) = 'WET '
               CHIDEP(3,ITYP) = 'DEPO'
               CHIDEP(4,ITYP) = 'SITI'
               CHIDEP(5,ITYP) = 'ON  '
               CHIDEP(6,ITYP) = '    '
               EMIFAC(ITYP) = 3600.0D0
               EMILBL(ITYP) = 'GRAMS/SEC'
               OUTLBL(ITYP) = 'GRAMS/M**2'
               PERLBL(ITYP) = 'GRAMS/M**2'
               OUTTYP(ITYP) = 'WDEP'
            END IF
         ELSE IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (DDEP) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'DRY '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'DDEP'
         IF (WDEP) THEN
            ITYP = 2
            CHIDEP(1,ITYP) = '    '
            CHIDEP(2,ITYP) = 'WET '
            CHIDEP(3,ITYP) = 'DEPO'
            CHIDEP(4,ITYP) = 'SITI'
            CHIDEP(5,ITYP) = 'ON  '
            CHIDEP(6,ITYP) = '    '
            EMIFAC(ITYP) = 3600.0D0
            EMILBL(ITYP) = 'GRAMS/SEC'
            OUTLBL(ITYP) = 'GRAMS/M**2'
            PERLBL(ITYP) = 'GRAMS/M**2'
            OUTTYP(ITYP) = 'WDEP'
         END IF
      ELSE IF (WDEP) THEN
         ITYP = 1
         CHIDEP(1,ITYP) = '    '
         CHIDEP(2,ITYP) = 'WET '
         CHIDEP(3,ITYP) = 'DEPO'
         CHIDEP(4,ITYP) = 'SITI'
         CHIDEP(5,ITYP) = 'ON  '
         CHIDEP(6,ITYP) = '    '
         EMIFAC(ITYP) = 3600.0D0
         EMILBL(ITYP) = 'GRAMS/SEC'
         OUTLBL(ITYP) = 'GRAMS/M**2'
         PERLBL(ITYP) = 'GRAMS/M**2'
         OUTTYP(ITYP) = 'WDEP'
      END IF

      EMICON = 1.0D+06

C --- Modify PLTFRM, PSTFRM and MXDFRM if needed for more than one output type
C     and for EXP format (note that FILE_FORMAT is set during PRESET).
      
      IF (NUMTYP .GT. 1 .AND. FILE_FORMAT .EQ. 'FIX') THEN
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            WRITE(PLTFRM,1009) NUMTYP+2
 1009       FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,A5,5X,',
     &             'A8,2X,10(F13.5,2X,I8.8,2X:))')
         ELSE
            WRITE(PLTFRM,1019) NUMTYP+2
 1019       FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),3X,A5,2X,A8,2X,A5,5X,',
     &             'A8,2X,I8)')
         END IF
         WRITE(PSTFRM,1029) NUMTYP+2
 1029    FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I8.8,2X,',
     &          'A8)')
         WRITE(MXDFRM,1039) NUMTYP+2
 1039    FORMAT('(',I1,'(1X,F13.5),3(1X,F8.2),2X,A6,2X,A8,2X,I4,2X,I3,',
     &          '2X,I8.8,2X,A8)')
      ELSE IF (NUMTYP .GT. 1 .AND. FILE_FORMAT .EQ. 'EXP') THEN
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            WRITE(PLTFRM,2009) NUMTYP
 2009       FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,',
     &             'A8,2X,A5,5X,A8,2X,10(E13.6,2X,I8.8,2X:))')
         ELSE
            WRITE(PLTFRM,2019) NUMTYP
 2019       FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),3X,A5,2X,',
     &             'A8,2X,A5,5X,A8,2X,I8)')
         END IF
         WRITE(PSTFRM,2029) NUMTYP
 2029    FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,A8,',
     &          '2X,I8.8,2X,A8)')
         WRITE(MXDFRM,2039) NUMTYP
 2039    FORMAT('(2(1X,F13.5),',I1,'(1X,E13.6),3(1X,F8.2),2X,A6,2X,A8,',
     &          '2X,I4,2X,I3,2X,I8.8,2X,A8)')
      END IF

 999  RETURN
      END

      SUBROUTINE AVETIM
C***********************************************************************
C                 AVETIM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Averaging Time Options From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Averaging Period Array and PERIOD Logical Switch
C
C        ERROR HANDLING:   Checks for Too Many Short Term Averages (>4);
C                          Checks for Invalid Averaging Periods, MOD(24,X) NE 0;
C                          Checks for Duplicate Short Term Averaging Periods
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K
      REAL    :: AVENUM
      CHARACTER (LEN = 8) :: KOPT

C     Variable Initializations
      MODNAM = 'AVETIM'

C     Check for No Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

C     First Check for Presence of PERIOD or ANNUAL Switch
      DO 10 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'PERIOD') THEN
            PERIOD = .TRUE.
         ELSE IF (KOPT .EQ. 'ANNUAL') THEN
            ANNUAL = .TRUE.
         END IF
 10   CONTINUE

C --- Check for Both PERIOD and ANNUAL
      IF (PERIOD .AND. ANNUAL) THEN
C        Write Error Message; both PERIOD and ANNUAL specified
         CALL ERRHDL(PATH,MODNAM,'E','294',KEYWRD)
      ELSE IF (PERIOD .OR. ANNUAL) THEN
C        Check for Too Many Averaging Periods
         IF (IFC .GT. NAVE+3) THEN
C           WRITE Error Message: Too Many Period Or Time Fields
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
          END IF
      ELSE
         IF (IFC .GT. NAVE+2) THEN
C           WRITE Error Message: Too Many Period Or Time Fields
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         END IF
      END IF

C     Loop Through Fields Again, Filling KAVE Array for Short Term Averages
      J = 0
      DO 20 I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .NE. 'PERIOD' .AND. KOPT .NE. 'ANNUAL') THEN
            IF (KOPT .NE. 'MONTH') THEN
               CALL STONUM(KOPT,8,AVENUM,IMIT)
               IF (IMIT .NE. 1) THEN
C                 Write Error Message:Invalid Numerical Field
                  CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
               END IF
C              Check for Valid Averaging Period
               IF ((MOD(24,NINT(AVENUM)).EQ.0 .AND.
     &                                   IMIT.EQ.1)) THEN
                  J = J + 1
                  IF (J .LE. NAVE) THEN
                     KAVE(J) = NINT(AVENUM)
                     WRITE(CHRAVE(J),'(I2,"-HR")') KAVE(J)
                     NUMAVE = J
C                    Check for Duplicate Averaging Periods
                     DO 15 K = J-1, 1, -1
                        IF (KAVE(J) .EQ. KAVE(K)) THEN
C                          WRITE Error Message    ! Duplicate Averaging Period
                           CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
                        END IF
 15                  CONTINUE
                  ELSE
C                    WRITE Error Message   ! Too Many Short Term Averaging Periods
                     WRITE(DUMMY,'(I8)') NAVE
                     CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
                  END IF
               ELSE
C                 WRITE Error Message      ! Invalid Averaging Period
                  CALL ERRHDL(PATH,MODNAM,'E','203','AVEPER')
               END IF
            ELSE
               J = J + 1
               IF (J .LE. NAVE) THEN
                  KAVE(J) = 720
                  MONTH = .TRUE.
                  CHRAVE(J) = 'MONTH'
                  NUMAVE = J
C                 Check for Duplicate Averaging Periods
                  DO K = J-1, 1, -1
                     IF (KAVE(J) .EQ. KAVE(K)) THEN
C                       WRITE Error Message    ! Duplicate Averaging Period
                        CALL ERRHDL(PATH,MODNAM,'E','211',KEYWRD)
                     END IF
                  END DO
               ELSE
C                 WRITE Error Message   ! Too Many Short Term Averaging Periods
                  WRITE(DUMMY,'(I8)') NAVE
                  CALL ERRHDL(PATH,MODNAM,'E','210',DUMMY)
               END IF
            END IF
         END IF
 20   CONTINUE

 999  RETURN
      END

      SUBROUTINE POLLID
C***********************************************************************
C                 POLLID Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Pollutant Identification Option
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Pollutant Identification Option
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'POLLID'

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

      POLLUT = FIELD(3)

 999  RETURN
      END

      SUBROUTINE EDECAY
C***********************************************************************
C                 EDECAY Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Exponential Decay Options
C
C        PROGRAMMER: Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Exponental Decay Options
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EDECAY'

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

C     Start To Get Decay Coef.
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

      IF (KEYWRD .EQ. 'HALFLIFE') THEN
         HAFLIF = DNUM
C        Calculate Decay Coef. by Halflife
         DECOEF = 0.693D0/HAFLIF
      ELSE IF (KEYWRD .EQ. 'DCAYCOEF') THEN
         DECOEF = DNUM
      END IF

C --- Check for Urban Regulatory Default for SO2; use L_PRESET_URBAN rather then 
C     URBAN to allow flexibility in order of keywords
      IF (DFAULT .AND. L_PRESET_URBAN .AND. POLLUT.EQ.'SO2') THEN
         IF (DECOEF .NE. 4.81D-5) THEN
C           WRITE Warning Message: Attempt to Override Regulatory Default
            CALL ERRHDL(PATH,MODNAM,'W','206','DCAYCOEF')
         END IF
         DECOEF = 4.81D-5
      ELSE IF (DFAULT) THEN
         IF (DECOEF .NE. 0.0D0) THEN
C           WRITE Warning Message: Attempt to Override Regulatory Default
            CALL ERRHDL(PATH,MODNAM,'W','206','DCAYCOEF')
         END IF
         DECOEF = 0.0D0
      ELSE IF (.NOT. DFAULT .AND. DECOEF .NE. 0.0D0) THEN
C        Set flag for use of non-DEFAULT option
         L_NonDFAULT = .TRUE.
      END IF

 999  RETURN
      END

      SUBROUTINE RUNNOT
C***********************************************************************
C                 RUNNOT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Option To RUN Or NOT From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Model RUN Logical Switch
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'RUNNOT'

      IF (IFC .EQ. 3) THEN
         IF (FIELD(3) .EQ. 'RUN') THEN
            RUN = .TRUE.
         ELSE IF (FIELD(3) .EQ. 'NOT') THEN
            RUN = .FALSE.
         ELSE
C           WRITE Error Message  ! Invalid Parameter
            CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message     ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE FLAGDF
C***********************************************************************
C                 FLAGDF Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Default Flagpole Receptor Height Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Default Flagpole Receptor Heights
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I
      DOUBLE PRECISION :: ZFLG

C     Variable Initializations
      MODNAM = 'FLAGDF'
      FLGPOL = .TRUE.

      IF (IFC .EQ. 3) THEN
         CALL STODBL(FIELD(3),ILEN_FLD,ZFLG,IMIT)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
         IF (ZFLG .GE. 0.0D0 .AND. IMIT .EQ. 1) THEN
            AZFLAG(:) = ZFLG
         ELSE IF (ZFLG .LT. 0.0D0 .AND. IMIT .EQ. 1) THEN
C            WRITE Error Message: Invalid Data. Negative value specified
             CALL ERRHDL(PATH,MODNAM,'E','209','ZFLAG')
         ELSE
C            WRITE Error Message: Invalid Parameter
             CALL ERRHDL(PATH,MODNAM,'E','203',KEYWRD)
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'W','205','ZFLAG=0.')
      END IF

 999  RETURN
      END

      SUBROUTINE EVNTFL
C***********************************************************************
C                 EVNTFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process EVENT File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: EVENT File Logical Switch and EVENT Filename
C
C        ERROR HANDLING:   Checks for No Parametes;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'EVNTFL'

      IF (IFC .EQ. 3) THEN
         EVENTS = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            EVFILE = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  EVFILE Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         EVPARM = 'DETAIL'
      ELSE IF (IFC .EQ. 4) THEN
         EVENTS = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            EVFILE = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  EVFILE Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         EVPARM = FIELD(4)
      ELSE IF (IFC .GT. 4) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message         ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         EVENTS = .TRUE.
         EVFILE = 'EVENTS.INP'
         EVPARM = 'DETAIL'
      END IF

C     Check for Invalid EVPARM
      IF (EVPARM .NE. 'SOCONT' .AND. EVPARM .NE. 'DETAIL') THEN
C        WRITE Warning Message         ! Invalid Parameter - Use Default
         CALL ERRHDL(PATH,MODNAM,'W','203','EVPARM')
      END IF

C     Open The EVENT Input File
      OPEN(UNIT=IEVUNT,FILE=EVFILE,STATUS='REPLACE',
     &     FORM='FORMATTED')

 999  RETURN
      END

      SUBROUTINE SAVEFL
C***********************************************************************
C                 SAVEFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process RESTART File Save Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'SAVEFL'

      IF (MULTYR) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         RSTSAV = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         SAVFL2 = SAVFIL
         INCRST = 1
      ELSE IF (IFC .EQ. 4) THEN
         RSTSAV = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         SAVFL2 = SAVFIL
         CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
         INCRST = NINT(FNUM)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
      ELSE IF (IFC .EQ. 5) THEN
         RSTSAV = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  SAVFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
         CALL STONUM(FIELD(4),ILEN_FLD,FNUM,IMIT)
         INCRST = NINT(FNUM)
         IF (IMIT .NE. 1) THEN
C           Write Error Message:Invalid Numerical Field
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         END IF
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            SAVFL2 = RUNST1(LOCB(5):LOCE(5))
         ELSE
C           WRITE Error Message:  SAVFL2 Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
      ELSE IF (IFC .GT. 5) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         RSTSAV = .TRUE.
         SAVFIL = 'SAVE.FIL'
         SAVFL2 = SAVFIL
         INCRST = 1
      END IF

 999  RETURN
      END

      SUBROUTINE INITFL
C***********************************************************************
C                 INITFL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process RESTART Initialization Input File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To change default filename to SAVE.FIL to match
C                    default name for SAVEFILE card.
C                    R.W. Brode, PES, Inc. - 6/20/95
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTINP Logical Switch and Re-start Input Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'INITFL'

      IF (MULTYR) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)
      ELSE IF (IFC .EQ. 3) THEN
         RSTINP = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            INIFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  INIFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
      ELSE IF (IFC .GT. 3) THEN
C        WRITE Error Message           ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C        WRITE Warning Message          ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         RSTINP = .TRUE.
         INIFIL = 'SAVE.FIL'
      END IF

 999  RETURN
      END

      SUBROUTINE ERRFIL
C***********************************************************************
C                 ERRFIL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Error Message File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Error Message File Logical Switch and ERRMSG Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'ERRFIL'

      IF (IFC .EQ. 3) THEN
         ERRLST = .TRUE.
C        Retrieve Included Filename as Character Substring to Maintain Case
         IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C           Retrieve Filename as Character Substring to Maintain Original Case
C           Also Check for Filename Larger Than ILEN_FLD Characters
            MSGFIL = RUNST1(LOCB(3):LOCE(3))
         ELSE
C           WRITE Error Message:  MSGFIL Field is Too Long
            WRITE(DUMMY,'(I8)') ILEN_FLD
            CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
            RETURN
         END IF
      ELSE IF (IFC .GT. 3) THEN
C*       WRITE Error Message                ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      ELSE
C*       WRITE Warning Message              ! No Parameters - Use Default Name
         CALL ERRHDL(PATH,MODNAM,'W','207',KEYWRD)
         ERRLST = .TRUE.
         MSGFIL = 'ERRORS.LST'
      END IF
C*#

 999  RETURN
      END

      SUBROUTINE DEBOPT
C***********************************************************************
C                 DEBOPT Module of AERMOD
C
C        PURPOSE: Process Debug Output File Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    September 30, 1993
C
C        MODIFIED:   Modified to allow user to specify debug output 
C                    only for PVMRM or deposition options on the 
C                    DEBUGOPT keyword, avoiding large ouput files 
C                    under the MODEL debug option. Debug output for 
C                    PVMRM and/or deposition options will still be 
C                    generated if the MODEL debug option is selected. 
C                    See AERMOD User's Guide Addendum for details 
C                    on the DEBUGOPT keyword.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Debug File Logical Switches and Filenames
C
C        ERROR HANDLING:   Checks for Too Few Parameters (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12, KOPT*8
      INTEGER :: I, IMOD, IMET, IPVM, IDEP

C     Variable Initializations
      MODNAM = 'DEBOPT'
      IMOD = 0
      IMET = 0
      IPVM = 0
      IDEP = 0

C     Check for Too Few or Too Many Parameters
      IF (IFC .LT. 3) THEN
C        WRITE Error Message     ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      ELSE IF (IFC .GT. 9) THEN
C        WRITE Warning Message   ! Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
      END IF

C     First Check for Presence of Debug Switches;
C     also save position to interpret optional 
C     filenames
      DO I = 3, IFC
         KOPT = FIELD(I)
         IF (KOPT .EQ. 'MODEL') THEN
            DEBUG = .TRUE.
            IMOD = I
         ELSE IF (KOPT .EQ. 'METEOR') THEN
            METEOR = .TRUE.
            IMET = I
         ELSE IF (KOPT .EQ. 'PVMRM') THEN
            PVMRMDBG = .TRUE.
            IPVM = I
         ELSE IF (KOPT .EQ. 'DEPOS') THEN
            DEPOSDBG = .TRUE.
            IDEP = I
         END IF
      END DO

C --- Check for PVMRM or DEPOS debug options without PVMRM or DEPOS
C     being used
      IF (PVMRMDBG .AND. .NOT.PVMRM) THEN
C        Write Error Message:  PVMRM debug without PVMRM option
         CALL ERRHDL(PATH,MODNAM,'E','194','PVMRMDBG')
      END IF
      IF (DEPOSDBG .AND. .NOT.DEPOS .AND. .NOT.DDEP .AND.
     &                                         .NOT.WDEP) THEN
C        Write Error Message:  DEPOS debug without deposition options
         CALL ERRHDL(PATH,MODNAM,'E','194','DEPOSDBG')
      END IF

C --- Check for user-specified filenames, which should immediately
C     follow the keyword option in the input file
      IF (DEBUG) THEN
         IF (IFC .GE. IMOD+1 .AND.
     &       FIELD(IMOD+1) .NE. 'METEOR' .AND. 
     &       FIELD(IMOD+1) .NE. 'PVMRM' .AND.
     &       FIELD(IMOD+1) .NE. 'DEPOS') THEN
C ---       Assign user-specified filename for the MODEL debug option
            DBGFIL = RUNST1(LOCB(IMOD+1):LOCE(IMOD+1))
         ELSE
C ---       Assign default MODEL debug filename
            DBGFIL = 'MODEL.DBG'
         END IF
      END IF
      
      IF (METEOR) THEN
         IF (IFC .GE. IMET+1 .AND.
     &       FIELD(IMET+1) .NE. 'MODEL' .AND. 
     &       FIELD(IMET+1) .NE. 'PVMRM' .AND.
     &       FIELD(IMET+1) .NE. 'DEPOS') THEN
C ---       Assign user-specified filename for the METEOR debug option
            DBMFIL = RUNST1(LOCB(IMET+1):LOCE(IMET+1))
         ELSE
C ---       Assign default METEOR debug filename
            DBMFIL = 'METEOR.DBG'
         END IF
      END IF
      
      IF (PVMRMDBG) THEN
         IF (IFC .GE. IPVM+1 .AND.
     &       FIELD(IPVM+1) .NE. 'MODEL' .AND. 
     &       FIELD(IPVM+1) .NE. 'METEOR' .AND.
     &       FIELD(IPVM+1) .NE. 'DEPOS') THEN
C ---       Assign user-specified filename for the PVMRM debug option
            DBPVFIL = RUNST1(LOCB(IPVM+1):LOCE(IPVM+1))
         ELSE
C ---       Assign default PVMRM debug filename
            DBPVFIL = 'PVMRM.DBG'
         END IF
      END IF
      
C --- Now check for DEPOS option; since DEPOS debug filenames are
C     hardwired, issue warning if user appears to have specified 
C     a filename
      IF (DEPOSDBG) THEN
         IF (IFC .GE. IDEP+1 .AND.
     &       FIELD(IDEP+1) .NE. 'MODEL' .AND. 
     &       FIELD(IDEP+1) .NE. 'METEOR' .AND.
     &       FIELD(IDEP+1) .NE. 'PVMRM') THEN
C ---       Write warning message regarding DEPOS debug filenames
            CALL ERRHDL(PATH,MODNAM,'W','203','DEPOSDBG')
         END IF
      END IF

C --- Open MODEL and METEOR debug files, if selected; 
C     note that PVMRM and DEPOS debug files are opened
C     elsewhere
      IF (DEBUG) THEN
C        Open debug output file
         DUMMY = 'DBGFIL'
         OPEN (UNIT=DBGUNT,FILE=DBGFIL,ERR=99,STATUS='REPLACE')
      END IF

      IF (METEOR) THEN
C        Open debug meteorology profile output file
         DUMMY = 'DBMFIL'
         OPEN (UNIT=DBMUNT,FILE=DBMFIL,ERR=99,STATUS='REPLACE')
      END IF

      GO TO 999

C     WRITE Error Message:  Error Opening File
 99   CALL ERRHDL(PATH,MODNAM,'E','500',DUMMY)

 999  RETURN
      END

      SUBROUTINE MYEAR
C***********************************************************************
C                 MYEAR Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process RESTART File Save Option
C                 From Runstream Input Image
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:  Treat the 'H6H' field as optional, with a warning
C                   that it is no longer required.
C                   R. W. Brode, U.S. EPA, OAQPS, AQMG, 10/19/2009
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: RSTSAV File Logical Switch and RESTART Filename
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'MYEAR'

      IF (RSTSAV .OR. RSTINP) THEN
C        WRITE Error Message:  Conflicting Options RE-START and MULTYEAR
         CALL ERRHDL(PATH,MODNAM,'E','145',KEYWRD)

      ELSE IF (.NOT. (POLLUT .EQ. 'PM10' .OR. POLLUT .EQ. 'PM-10' .OR.
     &                POLLUT .EQ. 'NO2'  .OR. POLLUT .EQ. 'SO2'   .OR.
     &                POLLUT .EQ. 'LEAD' .OR. POLLUT .EQ. 'OTHER' .OR.
     &                POLLUT .EQ. 'PM25' .OR. POLLUT .EQ. 'PM-2.5'.OR.
     &                POLLUT .EQ. 'PM-25'.OR. POLLUT .EQ. 'PM2.5') )THEN
C        WRITE Error Message:  Conflicting Options MULTYEAR For Wrong POLLUT
         CALL ERRHDL(PATH,MODNAM,'E','150',KEYWRD)

      ELSE IF (IFC .GE. 4 .AND. FIELD(3) .EQ. 'H6H') THEN
C ---    Write Warning Message:  The 'H6H' field is no longer required 
C        for the MULTYEAR keyword
         CALL ERRHDL(PATH,MODNAM,'W','352','Keyword ')
         IF (IFC .EQ. 4) THEN
            MULTYR = .TRUE.
            RSTSAV = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SAVFIL = RUNST1(LOCB(4):LOCE(4))
            ELSE
C              WRITE Error Message:  SAVFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
            SAVFL2 = SAVFIL
C ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
         ELSE IF (IFC .EQ. 5) THEN
            MULTYR = .TRUE.
            RSTSAV = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SAVFIL = RUNST1(LOCB(4):LOCE(4))
            ELSE
C              WRITE Error Message:  SAVFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
            SAVFL2 = SAVFIL
            RSTINP = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               INIFIL = RUNST1(LOCB(5):LOCE(5))
            ELSE
C              WRITE Error Message:  INIFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
C ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
         ELSE IF (IFC .GT. 5) THEN
C           WRITE Error Message           ! Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         END IF
      ELSE IF (IFC .GE. 3 .AND. FIELD(3) .NE. 'H6H') THEN
C ---    Process input parameters without the 'H6H' keyword
         IF (IFC .EQ. 3) THEN
            MULTYR = .TRUE.
            RSTSAV = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(3)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SAVFIL = RUNST1(LOCB(3):LOCE(3))
            ELSE
C              WRITE Error Message:  SAVFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
            SAVFL2 = SAVFIL
C ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
         ELSE IF (IFC .EQ. 4) THEN
            MULTYR = .TRUE.
            RSTSAV = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               SAVFIL = RUNST1(LOCB(3):LOCE(3))
            ELSE
C              WRITE Error Message:  SAVFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
            SAVFL2 = SAVFIL
            RSTINP = .TRUE.
C           Use Character Substring to Retrieve Filenames to Maintain Case
            IF ((LOCE(4)-LOCB(4)) .LE. (ILEN_FLD - 1) ) THEN
C              Retrieve Filename as Character Substring to Maintain Original Case
C              Also Check for Filename Larger Than ILEN_FLD Characters
               INIFIL = RUNST1(LOCB(4):LOCE(4))
            ELSE
C              WRITE Error Message:  INIFIL Field is Too Long
               WRITE(DUMMY,'(I8)') ILEN_FLD
               CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
               RETURN
            END IF
C ---       Value of INCRST is Set to 365 or 366 in SUB. MECARD
         ELSE IF (IFC .GT. 4) THEN
C           WRITE Error Message           ! Too Many Parameters
            CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         END IF
      ELSE IF (IFC .EQ. 3 .AND. FIELD(3) .EQ. 'H6H') THEN
C        WRITE Error Message           ! Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
      ELSE IF (IFC .LT. 3) THEN
C        WRITE Error Message           ! No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
      END IF

 999  RETURN
      END

      SUBROUTINE GDDEF
C***********************************************************************
C                 GDDEF Module of ISC3 Model
C
C        PURPOSE: Processes Dry Deposition Default Parameters for Gases
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    May 16, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Dry Deposition Reference Parameters for Gases
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'GDDEF'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 5) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 6) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Read Gas Dry Deposition Parameters
C     Change Them To Numbers
C     First Get Reactivity Value (fo)
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
C     Assign The Field
      Fo = DNUM

C     Now Get Fraction of Maximum Green LAI for Seasonal Category 2
      CALL STODBL(FIELD(4),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
C     Assign The Field
      FSEAS2 = DNUM

C     Now Get Fraction of Maximum Green LAI for Seasonal Category 5
      CALL STODBL(FIELD(5),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
C     Assign The Field
      FSEAS5 = DNUM

      IF (IFC .EQ. 6) THEN
C        Get the Reference Species (Optional)
         REFSPE = FIELD(6)
      ELSE
         REFSPE = '      '
      END IF

 999  RETURN
      END

      SUBROUTINE GDSEAS
C***********************************************************************
C                 GDSEAS Module of ISC3 Model
C
C        PURPOSE: Define Seasons for Gas Dry Deposition (per Wesely)
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    May 18, 2001
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Dry Deposition Reference Parameters for Gases
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I, J, ISEA_NDX

C     Variable Initializations
      MODNAM = 'GDSEAS'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 14) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      ISET = 0
      DO I = 3, IFC
C        Change Fields To Numbers
         CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. 12) THEN
               ISEA_NDX = NINT(FNUM)
               IF (ISEA_NDX .GE. 1 .AND. ISEA_NDX .LE. 5) THEN
                  ISEAS_GD(ISET) = ISEA_NDX
               ELSE
C                 WRITE Error Message    ! Season Index out-of-range
                  CALL ERRHDL(PATH,MODNAM,'E','380',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many Months Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO

 999  RETURN
      END

      SUBROUTINE GVSUBD
C***********************************************************************
C                 GVSUBD Module of ISC3 Model
C
C        PURPOSE: Processes Dry Deposition Reference Parameters for Gases
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    September 3, 1996
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: User-specified Dry Deposition Velocity for Gases
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'GVSUBD'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 3) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Read User-specified Dry Deposition Velocity
C     Change Them To Numbers
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF
C     Assign The Field
      USERVD = DNUM

C     Perform range/validity check
      IF (USERVD .LT. 0.0D0) THEN
C        Write Error Message:  Negative deposition velocity
         CALL ERRHDL(PATH,MODNAM,'E','209',' USERVD ')
      ELSE IF (USERVD .EQ. 0.0D0) THEN
C        Write Error Message:  Deposition velocity = 0.0
         CALL ERRHDL(PATH,MODNAM,'E','380','USERVD=0')
      ELSE IF (USERVD .GT. 0.05D0) THEN
C        Write Warning Message:  Large deposition velocity
         CALL ERRHDL(PATH,MODNAM,'W','320',' USERVD ')
      END IF

C     Set Logical Variable for User-specified Deposition Velocity
      LUSERVD = .TRUE.

 999  RETURN
      END

      SUBROUTINE GDLAND
C***********************************************************************
C                 GDLAND Module of ISC3 Model
C
C        PURPOSE: Define Land Use Categories by Direction for
C                 Gas Dry Deposition (per Wesely, et al, 2001)
C
C        PROGRAMMER: R. W. Brode, PES, Inc.
C
C        DATE:    December 30, 2002
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Dry Deposition Reference Parameters for Gases
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      INTEGER I, J, ILAND_NDX

C     Variable Initializations
      MODNAM = 'GDLAND'

C     Check the Number of Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 38) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      ISET = 0
      DO I = 3, IFC
C        Change Fields To Numbers
         CALL STONUM(FIELD(I),ILEN_FLD,FNUM,IMIT)
C        Check The Numerical Field
         IF (IMIT .EQ. -1) THEN
            CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
            CYCLE
         END IF
         DO J = 1, IMIT
            ISET = ISET + 1
C           Assign The Field
            IF (ISET .LE. 36) THEN
               ILAND_NDX = NINT(FNUM)
               IF (ILAND_NDX .GE. 1 .AND. ILAND_NDX .LE. 9) THEN
                  ILAND_GD(ISET) = ILAND_NDX
               ELSE
C                 WRITE Error Message    ! Land Use Index out-of-range
                  CALL ERRHDL(PATH,MODNAM,'E','380',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many Directions Input
               CALL ERRHDL(PATH,MODNAM,'E','234',KEYWRD)
            END IF
         END DO
      END DO

 999  RETURN
      END

      SUBROUTINE URBOPT
C***********************************************************************
C                 URBOPT Module of AERMOD Model
C
C        PURPOSE: Process Urban Option Inputs
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    June 11, 1996
C
C        MODIFIED:   Adjusted the limit for issuing a warning for urban 
C                    population out-of-range from 10,000 to 21,206, which 
C                    corresponds to a population density of 750/sq-km for
C                    an area within a 3km radius, consistent with the 
C                    Appendix W criterion for urban/rural determination 
C                    based on the population density.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 02/28/2011
C
C        MODIFIED:   To incorporate handling of non-'default' values of
C                    the optional urban roughness length other than 1m 
C                    as non-DFAULT.
C                    To prohibit use of urban roughness length .ne. 1.0m
C                    for regulatory DFAULT applications.  Modified limits
C                    on urban roughness length to generate warning messages.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 10/19/2009
C
C        MODIFIED:   To allow for multiple urban areas in a single
C                    model run, and adjust range for issuing warning
C                    regarding optional user-specified urban rounghness.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG - 12/07/06
C
C        MODIFIED:   To include optional parameter for urban roughness
C                    length.  Defaults to 1.0 meter if no value input.
C                    R.W. Brode, PES, Inc. - 09/10/02
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: URBPOP  [R]  Urban population
C                 URBNAM  [C]  Name of urban area (optional)
C                 URBZ0   [R]  Urban roughness lenght, m (optional)
C                                defaults to 1.0 meter
C
C        ERROR HANDLING:   Checks for Invalid Parameters;
C                          Checks for No Parameters;
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER I
      CHARACTER MODNAM*12, TEMPID*8

C     Variable Initializations
      MODNAM = 'URBOPT'

C     Determine Whether There Are Too Few Or Too Many Parameter Fields
      IF ((.NOT. L_MULTURB .AND. IFC .LT. 3) .OR.
     &          (L_MULTURB .AND. IFC .LT. 4)) THEN
C        WRITE Error Message: Missing Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF ((.NOT. L_MULTURB .AND. IFC .GT. 5) .OR.
     &               (L_MULTURB .AND. IFC .GT. 6)) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

      IF (.NOT. L_URBAN_ALL .AND. L_MULTURB) THEN
C        READ in the Urban ID for multiple urban areas
         TEMPID = FIELD(3)
         DO I = 1, NUMURB
            IF (TEMPID .EQ. URBID(I)) THEN
C              WRITE Error Message:  Urban ID already defined
               CALL ERRHDL(PATH,MODNAM,'E','303',TEMPID)
C              Exit to END
               GO TO 999
            END IF
         END DO

C        New Urban ID Defined, Increment Counters
         IURB = IURB + 1
         IF (IURB .GT. NURB) THEN
C           WRITE Error Message    ! Too Many Urban Areas Specified
            WRITE(DUMMY,'(I8)') NURB
            CALL ERRHDL(PATH,MODNAM,'E','285',DUMMY)
C           Exit to END
            GO TO 999
         END IF
         NUMURB = NUMURB + 1
         URBID(IURB) = TEMPID

         IF (IFC .GE. 4) THEN
            CALL STODBL(FIELD(4),ILEN_FLD,URBPOP(IURB),IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208','URB-POP')
            ELSE IF (URBPOP(IURB) .LT. 100.0D0) THEN
C ---          Urban population below about 90 will cause math error
C ---          Write Error Message:Invalid Value Specified
               CALL ERRHDL(PATH,MODNAM,'E','203','URB-POP')
            ELSE IF (URBPOP(IURB) .LT. 21206.0D0) THEN
C ---          Flag urban population below 21,206 as potentially out-of-range;
C              this value corresponds with a population density of 750/sq-km
C              across an area of 3km in radius, a criterion cited for urban 
C              classification in Section 7.2.3(d) of Appendix W.
               CALL ERRHDL(PATH,MODNAM,'W','320','URB-POP')
            END IF         
         END IF

         IF (IFC .GE. 5) THEN
C           Assign name of urban area (optional)
            URBNAM(IURB) = FIELD(5)
         END IF

         IF (IFC .EQ. 6) THEN
C           Assign value of urban roughness length (optional)
            CALL STODBL(FIELD(6),ILEN_FLD,URBZ0(IURB),IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208','URBAN_Z0')
            ELSE
               IF (DFAULT .AND. URBZ0(IURB) .NE. 1.0D0) THEN
C                 Write Warning Message: Non-default urban roughness length
                  CALL ERRHDL(PATH,MODNAM,'W','206','URBAN_Z0')
                  URBZ0(IURB) = 1.0D0
               ELSE IF (.NOT. DFAULT .AND. URBZ0(IURB) .NE. 1.0D0) THEN
C                 Set flag for use of non-DEFAULT option
                  L_NonDFAULT = .TRUE.
               END IF
               IF (URBZ0(IURB) .LT. 0.80D0) THEN
C                 Write Warning Message: Urban roughness out of range
                  WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
                  CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
               ELSE IF (URBZ0(IURB) .GT. 1.50D0 .AND.
     &                  URBZ0(IURB) .LT. 5.0D0) THEN
C                 Write Warning Message: Urban roughness out of range
                  WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
                  CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
               ELSE IF (URBZ0(IURB) .GE. 5.0D0) THEN
C                 Write Error Message: Urban roughness out of range
                  CALL ERRHDL(PATH,MODNAM,'E','380','URBAN Z0')
               END IF
            END IF
         ELSE
            URBZ0(IURB) = 1.0D0
         END IF

      ELSE IF (L_URBAN_ALL .AND. L_MULTURB) THEN
C        Write Error Message: URBANSRC ALL option with 
C        multiple URBAN areas
         CALL ERRHDL(PATH,MODNAM,'E','279','URBANSRC ALL')
         
      ELSE
C        Single Urban Area - Process Inputs without URBAN ID

         IURB = 1

         IF (IFC .GE. 3) THEN
            CALL STODBL(FIELD(3),ILEN_FLD,URBPOP(IURB),IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208','URB-POP')
            ELSE IF (URBPOP(IURB) .LT. 100.0D0) THEN
C ---          Urban population below about 90 will cause math error
C ---          Write Error Message:Invalid Value Specified
               CALL ERRHDL(PATH,MODNAM,'E','203','URB-POP')
            ELSE IF (URBPOP(IURB) .LT. 21206.0D0) THEN
C ---          Flag urban population below 21,206 as potentially out-of-range;
C              this value corresponds with a population density of 750/sq-km
C              across an area of 3km in radius, a criterion cited for urban 
C              classification in Section 7.2.3(d) of Appendix W.
               CALL ERRHDL(PATH,MODNAM,'W','320','URB-POP')
            END IF 
         END IF

         IF (IFC .GE. 4) THEN
C           Assign name of urban area (optional)
            URBNAM(IURB) = FIELD(4)
         END IF

         IF (IFC .EQ. 5) THEN
C           Assign value of urban roughness length (optional)
            CALL STODBL(FIELD(5),ILEN_FLD,URBZ0(IURB),IMIT)
            IF (IMIT .NE. 1) THEN
C              Write Error Message:Invalid Numerical Field
               CALL ERRHDL(PATH,MODNAM,'E','208','URBAN_Z0')
            ELSE
               IF (DFAULT .AND. URBZ0(IURB) .NE. 1.0D0) THEN
C                 Write Warning Message: Non-default urban roughness length
                  CALL ERRHDL(PATH,MODNAM,'W','206','URBAN_Z0')
                  URBZ0(IURB) = 1.0D0
               ELSE IF (.NOT. DFAULT .AND. URBZ0(IURB) .NE. 1.0D0) THEN
C                 Set flag for use of non-DEFAULT option
                  L_NonDFAULT = .TRUE.
               END IF
               IF (URBZ0(IURB) .LT. 0.80D0) THEN
C                 Write Warning Message: Urban roughness out of range
                  WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
                  CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
               ELSE IF (URBZ0(IURB) .GT. 1.50D0 .AND.
     &                  URBZ0(IURB) .LT. 5.0D0) THEN
C                 Write Warning Message: Urban roughness out of range
                  WRITE(DUMMY,'(F8.2)') URBZ0(IURB)
                  CALL ERRHDL(PATH,MODNAM,'W','353',DUMMY)
               ELSE IF (URBZ0(IURB) .GE. 5.0D0) THEN
C                 Write Error Message: Urban roughness out of range
                  CALL ERRHDL(PATH,MODNAM,'E','380','URBAN Z0')
               END IF
            END IF
         ELSE
            URBZ0(IURB) = 1.0D0
         END IF

         NUMURB = 1

      END IF

C     Assign Logical for Urban Option
      URBAN  = .TRUE.

 999  RETURN
      END

      SUBROUTINE O3VAL
C***********************************************************************
C                 O3VAL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes Ozone Value Option
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:    May 3, 2002
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'O3VAL'

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 4) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Start To Get Ozone Value
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

C     Assign value to O3BACK variable
      O3BACK = DNUM

C     Check for units of ozone value
      IF (IFC .EQ. 4) THEN
         IF (FIELD(4).EQ.'PPM' .OR. FIELD(4).EQ.'PPB' .OR.
     &       FIELD(4).EQ.'UG/M3') THEN
            O3VALUNITS = FIELD(4)
         ELSE
C           Write Error Message:  Invalid units for ozone value
            CALL ERRHDL(PATH,MODNAM,'E','203',' O3UNITS')
         END IF
      ELSE
         O3VALUNITS = 'UG/M3'
      END IF

      IF (O3VALUNITS .EQ. 'PPB') THEN
         O3BACK = O3BACK * O3_PPB
      ELSE IF (O3VALUNITS .EQ. 'PPM') then
         O3BACK = O3BACK * O3_PPM
      END IF

C     Check range of value
      IF (O3BACK .LE. 0.0D0 .OR. O3BACK .GT. 500.0D0) THEN
         CALL ERRHDL(PATH,MODNAM,'W','320',' O3BACK ')
      END IF

 999  RETURN
      END

      SUBROUTINE O3FIL
C***********************************************************************
C                 O3FIL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Process Ozone Data File Option
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    May 3, 2002
C
C        MODIFIED: Include checks for potential problem with Fortran
C                  format specifier.  Should include from 1 to 4 
C                  integers for date variables, and one real for 
C                  ozone data variable.  Warning message is issued
C                  if too many or too few integers/reals are specified.
C                  An error message may also be issued when reading
C                  the ozone file depending on the compiler options.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 04/13/2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        ERROR HANDLING:   Checks for No Parametes (uses default name);
C                          Checks for Too Many Parameters
C
C        CALLED FROM:   COCARD
C***********************************************************************
C
C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      
      INTEGER :: I, NumInt, NumReal

C     Variable Initializations
      MODNAM = 'O3FIL'
      NumInt  = 0
      NumReal = 0

C     Check The Number Of The Fields
      IF (IFC .LE. 2) THEN
C        Error Message: No Parameters
         CALL ERRHDL(PATH,MODNAM,'E','200',KEYWRD)
         GO TO 999
      ELSE IF (IFC .LT. 3) THEN
C        Error Message: Not Enough Parameters
         CALL ERRHDL(PATH,MODNAM,'E','201',KEYWRD)
         GO TO 999
      ELSE IF (IFC .GT. 5) THEN
C        Error Message: Too Many Parameters
         CALL ERRHDL(PATH,MODNAM,'E','202',KEYWRD)
         GO TO 999
      END IF

C     Set logical flag for hourly ozone file
      O3FILE = .TRUE.
      
C     Retrieve Ozone Data Filename as Character Substring to Maintain Case
      IF ((LOCE(3)-LOCB(3)) .LE. (ILEN_FLD - 1) ) THEN
C        Retrieve Filename as Character Substring to Maintain Original Case
C        Also Check for Filename Larger Than ILEN_FLD Characters
         OZONFL = RUNST1(LOCB(3):LOCE(3))
      ELSE
C        WRITE Error Message:  OZONFL Field is Too Long
C        Write error message and return
         WRITE(DUMMY,'(I8)') ILEN_FLD
         CALL ERRHDL(PATH,MODNAM,'E','291',DUMMY)
         GO TO 999
      END IF

C     Open The Ozone Input File
C     Open with ACTION='READ' to prevent overwrite and multiple access
      OPEN(UNIT=IO3UNT,FILE=OZONFL,STATUS='OLD',ERR=998,
     &     ACTION='READ',FORM='FORMATTED')

C     Check for units of ozone value
      IF (IFC .GE. 4) THEN
         IF (FIELD(4).EQ.'PPM' .OR. FIELD(4).EQ.'PPB' .OR.
     &       FIELD(4).EQ.'UG/M3') THEN
            O3FILUNITS = FIELD(4)
         ELSE
C           Write Error Message:  Invalid units for ozone value
            CALL ERRHDL(PATH,MODNAM,'E','203',' O3UNITS')
         END IF
      ELSE
         O3FILUNITS = 'UG/M3'
      END IF

      IF (IFC .EQ. 5) THEN
C        Check for Format String > ILEN_FLD PARAMETER
         IF ((LOCE(5)-LOCB(5)) .LE. (ILEN_FLD - 1)) THEN
C           Retrieve Met Format as Char. Substring 
            O3FORM = RUNST1(LOCB(5):LOCE(5))
C ---       Check for correct format specifiers for Ozone file;
C           should be 4 integers for date variables and 1 real for
C           ozone concentration; allow for 1 to 4 integers since
C           format statement may include 4I2, and also allow for 
C           either F, E, or D format for the data variable.
            DO I = 1, LEN_TRIM(O3FORM)
               IF (O3FORM(I:I).EQ.'I' .OR. 
     &             O3FORM(I:I).EQ.'i') THEN
                  NumInt  = NumInt  + 1
               ELSE IF (O3FORM(I:I).EQ.'F' .OR. 
     &                  O3FORM(I:I).EQ.'f') THEN
                  NumReal = NumReal + 1
               ELSE IF (O3FORM(I:I).EQ.'E' .OR. 
     &                  O3FORM(I:I).EQ.'e') THEN
                  NumReal = NumReal + 1
               ELSE IF (O3FORM(I:I).EQ.'D' .OR. 
     &                  O3FORM(I:I).EQ.'d') THEN
                  NumReal = NumReal + 1
               END IF
            END DO
            IF (NumInt.LT.1 .OR. NumInt.GT.4) THEN
C              WRITE Warning Message:  Potential problem with O3FORM
               WRITE(DUMMY,'(''NumInts= '',I3)') NumInt
               CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
            END IF
            IF (NumReal.NE.1) THEN
C              WRITE Warning Message:  Potential problem with O3FORM
               WRITE(DUMMY,'(''NumReal= '',I3)') NumReal
               CALL ERRHDL(PATH,MODNAM,'W','292',DUMMY)
            END IF
         ELSE
C           WRITE Error Message:  O3FORM Field is Too Long
            WRITE(DUMMY,'(''LEN='',I6)') LOCE(5)-LOCB(5)
            CALL ERRHDL(PATH,MODNAM,'E','292',DUMMY)
         END IF
      ELSE
C ---    Use 'free' format as the default
         O3FORM = 'FREE'
      END IF

      GO TO 999

C     Process Error Messages; error opening file
 998  CALL ERRHDL(PATH,MODNAM,'E','500',KEYWRD)

 999  RETURN
      END

      SUBROUTINE NO2EQ
C***********************************************************************
C                 NO2EQ Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes NO2 Equilibrium Value for PVMRM
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:    May 3, 2004
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'NO2EQ'

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

C     Start To Get Ozone Value
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

C     Assign value to NO2Equil variable
      NO2Equil = DNUM

C     Check range of value
      IF (NO2Equil .LT. 0.10D0 .OR. NO2Equil .GT. 1.0D0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','380','NO2Equil')
      END IF

 999  RETURN
      END


      SUBROUTINE NO2STK
C***********************************************************************
C                 NO2STK Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes NO2 Default In-stack Ratio Value for PVMRM
C
C        PROGRAMMER: Roger W. Brode
C
C        DATE:    September 7, 2005
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS:
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      INTEGER I
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'NO2STK'

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

C     Start To Get Ozone Value
      CALL STODBL(FIELD(3),ILEN_FLD,DNUM,IMIT)
C     Check The Numerical Field
      IF (IMIT .NE. 1) THEN
         CALL ERRHDL(PATH,MODNAM,'E','208',KEYWRD)
         GO TO 999
      END IF

C     Assign value to NO2Stack variable
      NO2Stack = DNUM

C     Check range of value
      IF (NO2Stack .LT. 0.0D0 .OR. NO2Stack .GT. 1.0D0) THEN
         CALL ERRHDL(PATH,MODNAM,'E','380','NO2Stack')
         GO TO 999
      END IF

      DO I = 1, NSRC
         ANO2_RATIO(I) = NO2Stack
      END DO

 999  RETURN
      END

      SUBROUTINE O3VALS
C***********************************************************************
C                 O3VALS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes User-specified Ozone concentrations, 
C                 based on same options for temporal variability 
C                 as the EMISFACT keyword for source emissions
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:       February 28, 2011
C
C        MODIFIED: Corrected the test for number of parameters to
C                  be .GE. 4 to allow for the ANNUAL option.
C                  R. W. Brode, U.S. EPA, OAQPS, AQMG, 12/19/2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IH, IL, ISDX, IO3MAX
      CHARACTER (LEN=12) :: LID, HID, LID1, LID2, HID1, HID2
      CHARACTER (LEN=ILEN_FLD) :: SOID
      LOGICAL FOUND, INGRP, RMARK

C     Variable Initializations
      MODNAM = 'O3VALS'
      
C --- Assign logical variable indicating that background concentrations
C     are specified
      L_O3VALUES = .TRUE.

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

      O3FLAG = FIELD(3)
      IF (O3FLAG .EQ. 'ANNUAL') THEN
         IO3MAX = 1
      ELSE IF (O3FLAG .EQ. 'SEASON') THEN
         IO3MAX = 4
      ELSE IF (O3FLAG .EQ. 'MONTH') THEN
         IO3MAX = 12
      ELSE IF (O3FLAG .EQ. 'HROFDY') THEN
         IO3MAX = 24
      ELSE IF (O3FLAG .EQ. 'WSPEED') THEN
         IO3MAX = 6
      ELSE IF (O3FLAG .EQ. 'SEASHR') THEN
         IO3MAX = 96
      ELSE IF (O3FLAG .EQ. 'HRDOW') THEN
         IO3MAX = 72
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG .EQ. 'HRDOW7') THEN
         IO3MAX = 168
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG .EQ. 'SHRDOW') THEN
         IO3MAX = 288
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG .EQ. 'SHRDOW7') THEN
         IO3MAX = 672
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG .EQ. 'MHRDOW') THEN
         IO3MAX = 864
         L_DayOfWeekOpts = .TRUE.
      ELSE IF (O3FLAG .EQ. 'MHRDOW7') THEN
         IO3MAX = 2016
         L_DayOfWeekOpts = .TRUE.
      ELSE
C        WRITE Error Message    ! Invalid O3FLAG Field Entered
         CALL ERRHDL(PATH,MODNAM,'E','203','QFLAG')
         GO TO 999 
      END IF
      
      IF (IO3MAX .LE. NO3F) THEN
         CALL O3FILL(IO3MAX)
      ELSE
C        WRITE Error Message     ! NBF Parameter Not Large Enough
         WRITE(DUMMY,'(''NO3F ='',I6)') NO3F
         CALL ERRHDL(PATH,MODNAM,'E','260',DUMMY)
      END IF

 999  RETURN
      END

      SUBROUTINE O3FILL(IO3MAX)
C***********************************************************************
C                 O3FILL Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Fill Variable Ozone Concentration Array
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Direction Specific Building Directions
C
C        CALLED FROM:   O3VALS
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: J, K, IO3MAX

C     Variable Initializations
      MODNAM = 'O3FILL'

      ISET = IO3SET

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
            IF (ISET .LE. IO3MAX) THEN
               O3VARY(ISET) = DNUM
               IF (DNUM .LT. 0.0D0) THEN
C                 WRITE Error Message:  Negative Value for BACKGRND
                  CALL ERRHDL(PATH,MODNAM,'E','209',KEYWRD)
               END IF
            ELSE
C              WRITE Error Message    ! Too Many BACKGRND Values Input
               CALL ERRHDL(PATH,MODNAM,'E','231','O3VALUES')
            END IF
         END DO
      END DO

      IO3SET = ISET

      RETURN
      END

      SUBROUTINE OZON_UNIT
C***********************************************************************
C                 OZON_UNIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Processes user-specified units for O3VALUES keyword 
C                 ozone concentrations
C
C        PROGRAMMER:  Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Input Runstream Image Parameters
C
C        OUTPUTS: Variable Emmission Rate Factors
C
C        CALLED FROM:   COCARD
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'OZON_UNIT'
      
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
         OzoneUnits = FIELD(3)
      ELSE
C        Write Error Message:  Invalid units for O3VALUES 
         CALL ERRHDL(PATH,MODNAM,'E','203','OzoneUnits')
      END IF

 999  RETURN
      END

