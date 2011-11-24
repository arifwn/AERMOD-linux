      SUBROUTINE INPSUM
C***********************************************************************
C                 INPSUM Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Input Data Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Print summary of Model Options and Met Data 
C                    to optional SUMMFILE.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12
      
C     Variable Initializations
      MODNAM = 'INPSUM'

C     Print Out The Model Options
      CALL PRTOPT(IOUNIT)
      IF (SUMMFILE) THEN
C ---    Include Model Options Summary in SUMMFILE
         CALL PRTOPT(ISUMUNT)
      END IF

C --- Print temporally varying ozone concentrations,
C     if specified on the O3VALUES keyword
      IF (L_O3VALUES) THEN
         CALL PRTO3VALS
      END IF

C     Print Out The Input Source Data
      CALL PRTSRC

C --- Print Out Background Concentrations, if specified
      IF (L_BACKGRND) THEN
         CALL PRTBKG
      END IF

      IF (.NOT. EVONLY) THEN
C        Print Out The Input Receptor Coordinates.
         CALL PRTREC

C        Check For Receptors Too Close To Sources (< 1m or < 3Lb)
         CALL CHKREC
      END IF

C     Print Out The Input Met Data Summary
      CALL PRTMET(IOUNIT)
      IF (SUMMFILE) THEN
C ---    Include Met Data Summary in SUMMFILE
         CALL PRTMET(ISUMUNT)
      END IF

      RETURN
      END

      SUBROUTINE PRTOPT(IOUNT)
C***********************************************************************
C                 PRTOPT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Model Options and Keyword Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To include note regarding special processing
C                    requirements for PM.25, 1hr NO2 and 1hr SO2.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 02/28/2011
C
C        MODIFIED:   To include DFAULT urban roughness length,
C                    provide a more complete summary of options
C                    when DFAULT option is not specified, clarify
C                    options and emissions/output units related to 
C                    deposition algorithms, address EVENT vs. 
C                    non-EVENT processing options, and provide a 
C                    more "refined" estimate of memory storage 
C                    requirements in STORE.
C                    Include output file unit calling argument to 
C                    support printing summary to main 'aermod.out' 
C                    file and to the optional SUMMFILE.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To remove reference to "upper bound" values for
C                    supersquat buildings, and to summarize inputs for
C                    multiple urban areas.
C                    Roger Brode, MACTEC (f/k/a PES), Inc., - 08/25/05
C
C        MODIFIED:   To Remove Summary of Keywords Table
C                    Roger Brode, PES, Inc.,  - 11/08/94
C
C        MODIFIED:   To add pathway 'TG' to process input file of Gridded
C                    Terrain data.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:   To add DDEP and WDEP parameters to CONC/DEPOS options
C                    to allow just the wet or just the dry deposition flux
C                    to be reported.  DEPOS now reports the sum of wet and
C                    dry fluxes.  Expand keywords to include input of wet
C                    scavenging coefficients (SO path).  Add override of
C                    Intermediate Terrain so that results are for only the
C                    simple terrain or the complex terrain model.
C                    D. Strimaitis, SRC - 11/8/93
C
C        MODIFIED:  To Include TOXXFILE Option - 9/29/92
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, ILMAX, IOUNT, NumBack
      CHARACTER (LEN=8) :: TEMP_BFLAG

C     Variable Initializations
      MODNAM = 'PRTOPT'

C     Summarize The Model Options
      CALL HEADER(IOUNT)
      WRITE(IOUNT,9041)
      IF (CONC) THEN
         WRITE(IOUNT,*) '**Model Is Setup For Calculation of ',
     &        'Average CONCentration Values.'
      END IF
      IF (DEPOS) THEN
         WRITE(IOUNT,*) '**Model Is Setup For Calculation of ',
     &        'Total DEPOSition Values.'
      END IF
      IF (DDEP) THEN
         WRITE(IOUNT,*) '**Model Is Setup For Calculation of ',
     &        'Dry DEPosition Values.'
      END IF
      IF (WDEP) THEN
         WRITE(IOUNT,*) '**Model Is Setup For Calculation of ',
     &        'Wet DEPosition Values.'
      END IF

      WRITE(IOUNT,9099)
      WRITE(IOUNT,*) '  --  DEPOSITION LOGIC  --'
      IF (LDGAS .AND. LUSERVD) THEN
         WRITE(IOUNT,*) '**User-specified GAS DRY DEPOSITION Velocity',
     &                  ' Provided.'
      ELSE IF (LDGAS .OR. LWGAS) THEN
         WRITE(IOUNT,*) '**GAS DEPOSITION Data Provided.'
      ELSE
         WRITE(IOUNT,*) '**NO GAS DEPOSITION Data Provided.'
      END IF
      IF (LDPART) THEN
         WRITE(IOUNT,*) '**PARTICLE DEPOSITION Data Provided.'
      ELSE
         WRITE(IOUNT,*) '**NO PARTICLE DEPOSITION Data Provided.'
      END IF
      IF (DDPLETE) THEN
         WRITE(IOUNT,*) '**Model Uses DRY DEPLETION.     DDPLETE  = ',
     &                    DDPLETE
         IF (ARDPLETE) THEN
            WRITE(IOUNT,*) 
     &                  '        with AREADPLT option    AREADPLT = ',
     &                    ARDPLETE
         ELSE IF (ROMBERG) THEN
            WRITE(IOUNT,*) 
     &                  '        with ROMBERG option     ROMBERG  = ',
     &                    ROMBERG
         END IF
      ELSE
         WRITE(IOUNT,*) '**Model Uses NO DRY DEPLETION.  DRYDPLT  = ',
     &                    DDPLETE
      END IF
      IF (WDPLETE) THEN
         WRITE(IOUNT,*) '**Model Uses WET DEPLETION.     WETDPLT  = ',
     &                    WDPLETE
      ELSE
         WRITE(IOUNT,*) '**Model Uses NO WET DEPLETION.  WETDPLT  = ',
     &                    WDPLETE
      END IF

      WRITE(IOUNT,9099)
      IF (.NOT.URBAN) THEN
         WRITE(IOUNT,*) '**Model Uses RURAL Dispersion Only.'
      ELSE IF (URBAN) THEN
         WRITE(IOUNT,9039) NURBSRC, NUMURB
9039     FORMAT(1X,'**Model Uses URBAN Dispersion Algorithm ',
     &                   'for the SBL for ',I5,' Source(s),'
     &            /'   for Total of ',I4,' Urban Area(s):')
         IF (NUMURB .GT. 1) THEN
            DO I = 1, NUMURB
               WRITE(IOUNT,9040) URBID(I), URBPOP(I), URBZ0(I)
9040           FORMAT(3X,'Urban ID = ',A8,' ;  Urban Population = ',
     &             F11.1,' ;  Urban Roughness Length = ',F6.3,' m')
            END DO
         ELSE
            WRITE(IOUNT,90401) URBPOP(1), URBZ0(1)
90401       FORMAT(3X,'Urban Population = ',
     &          F11.1,' ;  Urban Roughness Length = ',F6.3,' m')
         END IF
         IF (.NOT. L_UrbanTransition) THEN
            WRITE(IOUNT,90402)
90402       FORMAT(1X,'**Non-DFAULT option to ignore morning transition'
     &     ' from nighttime urban boundary layer (NoUrbTran) selected.')
         END IF
      END IF

      WRITE(IOUNT,9099)
      IF (DFAULT) THEN
         WRITE(IOUNT,*) '**Model Uses Regulatory DEFAULT Options:'
         WRITE(IOUNT,*) '        1. Stack-tip Downwash.'
         WRITE(IOUNT,*) '        2. Model Accounts for ELEVated ',
     &           'Terrain Effects.'
         WRITE(IOUNT,*) '        3. Use Calms Processing ',
     &           'Routine.'
         WRITE(IOUNT,*) '        4. Use Missing Data ',
     &           'Processing Routine.'
         IF (URBAN .AND. POLLUT .EQ. 'SO2') THEN
            WRITE(IOUNT,*) '        5. Half-life of 4 hrs for',
     &              ' URBAN SO2.'
         ELSE IF (URBAN .AND. POLLUT .NE. 'SO2') THEN
            WRITE(IOUNT,*) '        5. No Exponential Decay for',
     &              ' URBAN/Non-SO2.'
         ELSE
            WRITE(IOUNT,*) '        5. No Exponential Decay.'
         END IF
         IF (URBAN) THEN
            IF (POLLUT .EQ. 'NO2') THEN
               WRITE(IOUNT,*) '        6. Full Conversion Assumed ',
     &                 'for NO2.'
               WRITE(IOUNT,*) '        7. Urban Roughness Length of ',
     &                 '1.0 Meter Assumed.'
            ELSE
               WRITE(IOUNT,*) '        6. Urban Roughness Length of ',
     &                 '1.0 Meter Assumed.'
            END IF
         ELSE IF (POLLUT .EQ. 'NO2') THEN
            WRITE(IOUNT,*) '        6. Full Conversion Assumed ',
     &              'for NO2.'
         END IF
      ELSE
         WRITE(IOUNT,*) '**Model Allows User-Specified Options:'
         IF (NOSTD) THEN
            WRITE(IOUNT,*) '        1. Not Use Stack-tip ',
     &           'Downwash.'
         ELSE
            WRITE(IOUNT,*) '        1. Stack-tip Downwash.'
         END IF
         IF (FLATSRCS) THEN
            WRITE(IOUNT,*) '        2. Allow FLAT/ELEV Terrain Option ',
     &              'by Source,'
            WRITE(IOUNT,9049) NUMFLAT, NUMSRC-NUMFLAT
         ELSE IF (FLAT) THEN
            WRITE(IOUNT,*) '        2. Model Assumes Receptors on ',
     &              'FLAT Terrain.'
         ELSE IF (ELEV) THEN
            WRITE(IOUNT,*) '        2. Model Accounts for ELEVated ',
     &              'Terrain Effects.'
         END IF
         WRITE(IOUNT,*) '        3. Use Calms Processing ',
     &           'Routine.'
         WRITE(IOUNT,*) '        4. Use Missing Data ',
     &           'Processing Routine.'
         IF (URBAN .AND. POLLUT.EQ.'SO2' .AND.
     &                  (DABS(DECOEF-4.81D-5) .LE. 5.0D-8)) THEN
            WRITE(IOUNT,*) '        5. Half-life of 4 hrs for',
     &              ' URBAN SO2.'
         ELSE IF (URBAN .AND. POLLUT.EQ.'SO2' .AND.
     &                  (DABS(DECOEF-4.81D-5) .GT. 5.0D-8)) THEN
            WRITE(IOUNT,*) '        5. Non-DFAULT Half-life for',
     &              ' URBAN SO2.'
         ELSE IF (DECOEF .NE. 0.0D0) THEN
            WRITE(IOUNT,*) '        5. Non-DFAULT Exponential',
     &              ' Decay.'
         ELSE
            WRITE(IOUNT,*) '        5. No Exponential Decay.'
         END IF
         IF (PVMRM) THEN
            WRITE(IOUNT,*) '        6. Plume Volume Molar Ratio ',
     &              'Method (PVMRM) Used for NO2 Conversion'
            WRITE(IOUNT,9090) NO2Equil
9090        FORMAT('            with an Equilibrium NO2/NOx ',
     &              'Ratio of ',F6.3)
            WRITE(IOUNT,9091) NO2Stack
9091        FORMAT('            and with a Default In-stack ',
     &              'Ratio of ',F6.3)
            IF (PSDCREDIT) THEN
               WRITE(IOUNT,9099)
               WRITE(IOUNT,*)'           BETA Option for Calculating ',
     &                        'PSD Increment'
               WRITE(IOUNT,*)'           Consumption with PSD Credits',
     &                        ' Selected.'
            END IF
         ELSE IF (OLM) THEN
            WRITE(IOUNT,*) '        6. Ozone Limiting Method (OLM) ',
     &              'Used for NO2 Conversion'
            WRITE(IOUNT,9090) NO2Equil
         ELSE IF (POLLUT .EQ. 'NO2') THEN
            WRITE(IOUNT,*) '        6. Full Conversion Assumed for ',
     &              'NO2.'
         END IF
         IF (.NOT.PVMRM .AND. .NOT.OLM .AND. POLLUT.NE.'NO2') THEN
            IF (URBAN) THEN
               IF (MAXVAL(URBZ0).NE.1.0D0 .OR. 
     &             MINVAL(URBZ0).NE.1.0D0) THEN
                  WRITE(IOUNT,*) '        6. Non-DFAULT Urban ',
     &                  'Roughness Length(s) Used.'
               ELSE
                  WRITE(IOUNT,*) '        6. Urban Roughness Length ',
     &                  'of 1.0 Meter Used.'
               END IF
               IF (BETA .AND. (NUMCAP.GT.0 .OR. NUMHOR.GT.0) ) THEN
                  WRITE(IOUNT,*) '        7. BETA Option for Capped &',
     &                            ' Horiz Stacks Selected With:'
                  WRITE(IOUNT,9099)
                  WRITE(IOUNT,9092) NUMCAP, NUMHOR
9092              FORMAT(12X,I6,' Capped Stack(s); and ',I6,
     &                          ' Horiz Stack(s)')
               END IF
            ELSE
               IF (BETA .AND. (NUMCAP.GT.0 .OR. NUMHOR.GT.0) ) THEN
                  WRITE(IOUNT,*) '        6. BETA Option for Capped &',
     &                            ' Horiz Stacks Selected With:'
                  WRITE(IOUNT,9099)
                  WRITE(IOUNT,9092) NUMCAP, NUMHOR
               END IF
            END IF
         ELSE 
            IF (URBAN) THEN
               IF (MAXVAL(URBZ0).NE.1.0D0 .OR. 
     &             MINVAL(URBZ0).NE.1.0D0) THEN
                  WRITE(IOUNT,*) '        7. Non-DFAULT Urban ',
     &                  'Roughness Length(s) Used.'
               ELSE
                  WRITE(IOUNT,*) '        7. Urban Roughness Length ',
     &                  'of 1.0 Meter Used.'
               END IF
               IF (.NOT. L_UrbanTransition) THEN
                  IF (BETA .AND. (NUMCAP.GT.0 .OR. NUMHOR.GT.0) ) THEN
                     WRITE(IOUNT,*) '        8. BETA Option for Capped',
     &                              ' & Horiz Stacks Selected With:'
                     WRITE(IOUNT,9099)
                     WRITE(IOUNT,9092) NUMCAP, NUMHOR
                     WRITE(IOUNT,*) '        9. Non-DFAULT Option ',
     &                             'to Ignore Urban Morning Transition.'
                  ELSE
                     WRITE(IOUNT,*) '        8. Non-DFAULT Option ',
     &                             'to Ignore Urban Morning Transition.'
                  END IF
               END IF
            ELSE
               IF (BETA .AND. (NUMCAP.GT.0 .OR. NUMHOR.GT.0) ) THEN
                  WRITE(IOUNT,*) '        7. BETA Option for Capped &',
     &                            ' Horiz Stacks Selected With:'
                  WRITE(IOUNT,9099)
                  WRITE(IOUNT,9092) NUMCAP, NUMHOR
               END IF
            END IF
         END IF
      END IF
      
      IF (NOWARN .OR. NOCHKD .OR. SCREEN .OR. FASTALL .OR. FASTAREA .OR.
     &                                        L_WARNCHKD .OR. SCIM) THEN
         WRITE(IOUNT,9099)
         WRITE(IOUNT,*) '**Other Options Specified:'
      END IF
      IF (NOCHKD) THEN
         WRITE(IOUNT,*) '        NOCHKD   - Suppresses checking',
     &                   ' of date sequence in meteorology files'
      ELSE IF (L_WARNCHKD) THEN
         WRITE(IOUNT,*) '        WARNCHKD - Issues warning messages',
     &                   ' for records out of sequence'
         WRITE(IOUNT,*) '                   in meteorology files'
      END IF
      IF (NOWARN) THEN
         WRITE(IOUNT,*) '        NOWARN   - Suppresses writing',
     &                   ' of warning messages in main print file'
      END IF
      IF (FASTALL) THEN
         WRITE(IOUNT,*) '        FASTALL  - Use effective sigma-y to',
     &                   ' optimize meander for '
         WRITE(IOUNT,*) '                   POINT and VOLUME',
     &                   ' sources, and hybrid approach'
         WRITE(IOUNT,*) '                   to optimize AREA sources',
     &                   ' (formerly TOXICS option)'
      ELSE IF (FASTAREA) THEN
         WRITE(IOUNT,*) '        FASTAREA - Use hybrid approach to',
     &                   ' optimize AREA sources'
         WRITE(IOUNT,*) '                   (formerly TOXICS option)'
      END IF
      IF (SCIM) THEN
         WRITE(IOUNT,*) '        SCIM     - Use Sampled',
     &                   ' Chronological Input Model (SCIM) option'
      END IF
      IF (SCREEN) THEN
         WRITE(IOUNT,*) '        SCREEN   - Use screening option ',
     &                   'which forces calculation of centerline values'
      END IF
C*#
      WRITE(IOUNT,9099)
      IF (FLGPOL) THEN
         WRITE(IOUNT,*) '**Model Accepts FLAGPOLE Receptor Heights.'
      ELSE
         WRITE(IOUNT,*) '**Model Assumes No FLAGPOLE Receptor Heights.'
      END IF

C     Model Sources And Receptors Summary
      WRITE(IOUNT,9099)
      IF (PERIOD) THEN
         IF (NUMAVE .GT. 0) THEN
            WRITE(IOUNT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
            WRITE(IOUNT,9043)
         ELSE
            WRITE(IOUNT,9045)
         END IF
      ELSE IF (ANNUAL) THEN
         IF (NUMAVE .GT. 0) THEN
            WRITE(IOUNT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
            WRITE(IOUNT,9143)
         ELSE
            WRITE(IOUNT,9145)
         END IF
      ELSE
         WRITE(IOUNT,9042) NUMAVE, (CHRAVE(I),I=1,NUMAVE)
      END IF

C     Write Out Numbers of Sources, Groups, and Receptors for This Run
      WRITE(IOUNT,9099)
      IF (EVONLY) THEN
         WRITE(IOUNT,9046) NUMSRC, NUMGRP, NUMEVE
      ELSE IF (.NOT. EVONLY) THEN
         WRITE(IOUNT,9044) NUMSRC, NUMGRP, NUMREC
      END IF
      
C --- Indicate whether background concentrations are included in this run
      IF (L_BACKGRND) THEN
C        Background concentrations; determine how many source groups
C        include background
         NumBack = 0
         DO I = 1, NUMGRP
            IF (GRP_BACK(I)) THEN
               NumBack = Numback + 1
            END IF
         END DO
         IF (L_HourlyBackgrnd) THEN
            TEMP_BFLAG = 'HOURLY'
            IF (IBKGRD .GT. 0) THEN
               WRITE(IOUNT,99047) TEMP_BFLAG, NumBack, BFLAG
            ELSE
               WRITE(IOUNT,9047) TEMP_BFLAG, NumBack
            END IF
         ELSE
            WRITE(IOUNT,9047) BFLAG, NumBack
         END IF
      END IF

C     Write Out Pollutant Type
      WRITE(IOUNT,9099)
      WRITE(IOUNT,9048) POLLUT
C --- Include note regarding 24-hr PM2.5, 1-hr NO2 and 1-hr SO2 processing
      IF (PM25AVE) THEN
         WRITE(IOUNT,99090) 
      ELSE IF (NO2AVE) THEN
         WRITE(IOUNT,99091) 
      ELSE IF (SO2AVE) THEN
         WRITE(IOUNT,99092) 
      END IF

C     Model Run OR Not Options
      WRITE(IOUNT,9099)
      IF (RUN) THEN
         WRITE(IOUNT,*) '**Model Set To Continue RUNning After the ',
     &         'Setup Testing.'
      ELSE
         WRITE(IOUNT,*) '**Model Will NOT Run After the ',
     &         'Setup Testing.'
      END IF

C     Model Output Options Setting Summary
      WRITE(IOUNT,9099)
      WRITE(IOUNT,9070)
      IF (EVONLY) THEN
C        Write output option for EVENT processing
         IF (SOCONT) THEN
            WRITE(IOUNT,98071)
         ELSE IF (DETAIL) THEN
            WRITE(IOUNT,98072)
         END IF
      ELSE
C        Write output options for non-EVENT processing
         IF (PERIOD) THEN
C           PERIOD Averages by Receptor Are Output
            WRITE(IOUNT,9071)
         ELSE IF (ANNUAL) THEN
C           ANNUAL Averages by Receptor Are Output
            WRITE(IOUNT,9171)
         END IF
         IF (IOSTAT(2) .GT. 0) THEN
C           RECTABLE Keyword Used
            WRITE(IOUNT,9072)
         END IF
         IF (IOSTAT(3) .GT. 0) THEN
C           MAXTABLE Keyword Used
            WRITE(IOUNT,9073)
         END IF
         IF (IOSTAT(4) .GT. 0) THEN
C           DAYTABLE Keyword Used
            WRITE(IOUNT,9074)
         END IF
         IF (IOSTAT(5) .GT. 0) THEN
C           MAXIFILE Keyword Used
            WRITE(IOUNT,9075)
         END IF
         IF (IOSTAT(6) .GT. 0) THEN
C           POSTFILE Keyword Used
            WRITE(IOUNT,9076)
         END IF
         IF (IOSTAT(7) .GT. 0) THEN
C           PLOTFILE Keyword Used
            WRITE(IOUNT,9077)
         END IF
         IF (IOSTAT(8) .GT. 0) THEN
C           TOXXFILE Keyword Used
            WRITE(IOUNT,9078)
         END IF
         IF (IOSTAT(9) .GT. 0) THEN
C           SEASONHR Keyword Used
            WRITE(IOUNT,99071)
         END IF
         IF (IOSTAT(10) .GT. 0) THEN
C           RANKFILE Keyword Used
            WRITE(IOUNT,99072)
         END IF
         IF (IOSTAT(11) .GT. 0) THEN
C           EVALFILE Keyword Used
            WRITE(IOUNT,99073)
         END IF
         IF (IOSTAT(14) .GT. 0) THEN
C           MAXDAILY Keyword Used
            WRITE(IOUNT,99173)
         END IF
         IF (IOSTAT(15) .GT. 0) THEN
C           MXDYBYYR Keyword Used
            WRITE(IOUNT,99273)
         END IF
         IF (IOSTAT(16) .GT. 0) THEN
C           MAXDCONT Keyword Used
            WRITE(IOUNT,99373)
         END IF
         IF (IOSTAT(12) .GT. 0) THEN
C           SUMMFILE Keyword Used
            WRITE(IOUNT,99074)
         END IF
      END IF
      
C --- Check for user-specified option for exponential-format outputs
      IF ( FILE_FORMAT .EQ. 'EXP' .AND.
     &    (MXFILE .OR. PPFILE .OR. RKFILE .OR. ANPOST .OR. ANPLOT .OR. 
     &     SEASONHR) ) THEN
         WRITE(IOUNT,9099)
         WRITE(IOUNT,99075)
      END IF

C     Write Explanatory Note About Calm and Missing Flags
      IF (CLMPRO .OR. MSGPRO) THEN
         WRITE(IOUNT,9099)
         WRITE(IOUNT,9079) CHIDEP(3,1)
      END IF

C     Model Misc. Information
      WRITE(IOUNT,9099)
      WRITE(IOUNT,9050) ZBASE, DECOEF, ROTANG
C --- Write out emission and output units
      IF (NUMTYP .EQ. 1) THEN
C ---    Only one output type; write out units without label
         WRITE(IOUNT,9055) EMILBL(1), EMIFAC(1), OUTLBL(1)
      ELSE IF (CONC) THEN
C ---    More than one output type including CONC;
C        Write out CONC units first with label, followed by
C        deposition units (for DEPOS, DDEP and/or WDEP)
         WRITE(IOUNT,90551) EMILBL(1), EMIFAC(1), OUTLBL(1)
         WRITE(IOUNT,90552) EMILBL(2), EMIFAC(2), OUTLBL(2)
      ELSE
C ---    More than one output type but no CONC;
C        Write out units for deposition without label
         WRITE(IOUNT,9055) EMILBL(1), EMIFAC(1), OUTLBL(1)
      END IF
      
      IF (LUSERVD) THEN
C        Write user-specified gas dry deposition velocity (GASDEPVD)
         WRITE(IOUNT,9099)
         WRITE(IOUNT,9056)  USERVD
      END IF

      IF (.NOT. EVONLY) THEN
C        Write Allocated Storage Requirements (est.)
         WRITE(IOUNT,9099)
         WRITE(IOUNT,9057) STORE
      END IF

C     Model I/O Setting Summary
      WRITE(IOUNT,9099)
      ILMAX = MIN( 96, ILEN_FLD )
      IF (INPFIL .NE. ' ' .OR. OUTFIL .NE. ' ') THEN
         WRITE(IOUNT,9080) INPFIL(1:ILMAX), OUTFIL(1:ILMAX)
      END IF
      IF (RSTINP .AND. .NOT.MULTYR) THEN
         WRITE(IOUNT,9081) INIFIL(1:ILMAX)
      ELSE IF (RSTINP .AND. MULTYR) THEN
         WRITE(IOUNT,99081) INIFIL(1:ILMAX)
      END IF
      IF (RSTSAV) WRITE(IOUNT,9082) SAVFIL(1:ILMAX)
      IF (ERRLST) WRITE(IOUNT,9083) MSGFIL(1:ILMAX)
      IF (EVENTS) WRITE(IOUNT,9084) EVFILE(1:ILMAX)
      IF (SUMMFILE) WRITE(IOUNT,9085) SUMFIL(1:ILMAX)

      IF (MULTYR) THEN
C ---    Write message regarding MULTYEAR applications      
         WRITE(IOUNT,9099)
         WRITE(IOUNT,*) '**This Run is Part of a Multi-year (MULTYEAR)',
     &                   ' Application.'
         IF (PERIOD) THEN
            WRITE(IOUNT,*) '  NOTE:  The PERIOD Results Table Reflects',
     &                   ' Current Period Only;'
            WRITE(IOUNT,*) '         The Overall Maximum PERIOD',
     &                   ' Results Table and'
         ELSE IF (ANNUAL) THEN
            WRITE(IOUNT,*) '  NOTE:  Both ANNUAL Average Results and'
         ELSE
            WRITE(IOUNT,*) '  NOTE:'
         END IF
         WRITE(IOUNT,*) '         Short Term Results are Cumulative',
     &                   ' Across All Years Processed.'
      END IF

 9041 FORMAT(/44X,'***     MODEL SETUP OPTIONS SUMMARY       ***'/
     &       63(' -')/)
 9042 FORMAT(1X,'**Model Calculates ',I2,' Short Term Average(s)',
     &       ' of:  ',9(A5,2X,:))
 9043 FORMAT(1X,'    and Calculates PERIOD Averages')
 9045 FORMAT(1X,'**Model Calculates PERIOD Averages Only')
 9143 FORMAT(1X,'    and Calculates ANNUAL Averages')
 9145 FORMAT(1X,'**Model Calculates ANNUAL Averages Only')
 9044 FORMAT(1X,'**This Run Includes: ',I6,' Source(s);  ',I6,
     &       ' Source Group(s); and  ',I6,' Receptor(s)')
 9046 FORMAT(1X,'**This Run is for EVENT Processing.',
     &      /1X,'**         and Includes: ',I6,' Source(s);  ',I6,
     &       ' Source Group(s); and  ',I6,' Event(s)')
99047 FORMAT(/1X,'**This Run Includes BACKGRND Concentrations ',
     &       'Varying by ',A8,' for ',I6,' Source Group(s);'
     &       /6X,'Missing HOURLY BACKGRND Concentrations are ',
     &       'Filled Based on ',A8,' BACKGRND Values')
 9047 FORMAT(/1X,'**This Run Includes BACKGRND Concentrations ',
     &       'Varying by ',A8,' for ',I6,' Source Group(s)')
 9048 FORMAT(1X,'**The Model Assumes A Pollutant Type of:  ',A8)
 9049 FORMAT(12X,'with ',I6,' FLAT and ',I6,' ELEV Source(s).')
 9050 FORMAT(1X,'**Misc. Inputs:  Base Elev. for Pot. Temp. Profile ',
     &       '(m MSL) = ',F8.2,' ;  Decay Coef. = ',G12.4,' ;',
     &       '  Rot. Angle = ',F7.1)
 9055 FORMAT(18X,'Emission Units = ',A40,' ;  Emission Rate Unit ',
     &       'Factor = ',G13.5,
     &      /18X,'Output Units   = ',A40)
90551 FORMAT(2X,'Concentration:',2X,'Emission Units = ',A40,' ;  ',
     &       'Emission Rate Unit Factor = ',G13.5,
     &      /18X,'Output Units   = ',A40)
90552 FORMAT(5X,'Deposition:',2X,'Emission Units = ',A40,' ;  ',
     &       'Emission Rate Unit Factor = ',G13.5,
     &      /18X,'Output Units   = ',A40)
 9056 FORMAT(18X,'User-Specified Dry Deposition Velocity for Gases ',
     &       '(m/s) = ',G13.5)
 9057 FORMAT(1X,'**Approximate Storage Requirements of Model = ',F8.1,
     &       ' MB of RAM.')
 9070 FORMAT(1X,'**Output Options Selected:')
98071 FORMAT(10X,'Model Outputs Source Contribution Information Only ',
     &           'for EVENT Processing (SOCONT Option)')
98072 FORMAT(10X,'Model Outputs Hourly Average Values for Each Source ',
     &           'for EVENT Processing (DETAIL Option)')
 9071 FORMAT(10X,'Model Outputs Tables of PERIOD Averages by Receptor')
 9171 FORMAT(10X,'Model Outputs Tables of ANNUAL Averages by Receptor')
 9072 FORMAT(10X,'Model Outputs Tables of Highest Short Term Values by',
     &       ' Receptor (RECTABLE Keyword)')
 9073 FORMAT(10X,'Model Outputs Tables of Overall Maximum Short Term',
     &       ' Values (MAXTABLE Keyword)')
 9074 FORMAT(10X,'Model Outputs Tables of Concurrent Short Term Values',
     &       ' by Receptor for Each Day Processed (DAYTABLE Keyword)')
 9075 FORMAT(10X,'Model Outputs External File(s) of Threshold',
     &       ' Violations (MAXIFILE Keyword)')
 9076 FORMAT(10X,'Model Outputs External File(s) of Concurrent Values',
     &       ' for Postprocessing (POSTFILE Keyword)')
 9077 FORMAT(10X,'Model Outputs External File(s) of High Values for',
     &       ' Plotting (PLOTFILE Keyword)')
 9078 FORMAT(10X,'Model Outputs External File(s) of Values for Input',
     &       ' to TOXX Model (TOXXFILE Keyword)')
99071 FORMAT(10X,'Model Outputs External File(s) of Values by Season',
     &       ' and Hour-of-Day (SEASONHR Keyword)')
99072 FORMAT(10X,'Model Outputs External File(s) of Ranked Values',
     &       ' (RANKFILE Keyword)')
99073 FORMAT(10X,'Model Outputs External File(s) of Arc-maximum Values',
     &       ' for Evaluation Purposes (EVALFILE Keyword)')
99173 FORMAT(10X,'Model Outputs External File(s) of Maximum Daily 1-hr',
     &       ' Values by Day (MAXDAILY Keyword)')
99273 FORMAT(10X,'Model Outputs External File(s) of Maximum Daily 1-hr',
     &       ' Values by Year (MXDYBYYR Keyword)')
99373 FORMAT(10X,'Model Outputs External File(s) of Contributions',
     &       ' to Maximum Daily Values Paired in Time & Space',
     &       ' (MAXDCONT Keyword)')
99074 FORMAT(10X,'Model Outputs Separate Summary File of High Ranked',
     &       ' Values (SUMMFILE Keyword)')
99075 FORMAT(10X,'NOTE: Option for EXPonential format used in',
     &       ' formatted output result files (FILEFORM Keyword)')
 9079 FORMAT(1X,'**NOTE:  The Following Flags May Appear Following ',
     &       A4,' Values:  c for Calm Hours',
     &               /65X,'m for Missing Hours',
     &               /65X,'b for Both Calm and Missing Hours')
 9080 FORMAT(1X,'**Input Runstream File:          ',A96,
     &      /1X,'**Output Print File:             ',A96/)
 9081 FORMAT(1X,'**NOTE: This Run was Restarted (INITFILE Keyword):',
     &      /1X,'**File for Initializing Arrays:  ',A96)
99081 FORMAT(1X,'**NOTE: This Run was Restarted (MULTYEAR Keyword):',
     &      /1X,'**File for Initializing Arrays:  ',A96)
 9082 FORMAT(1X,'**File for Saving Result Arrays: ',A96)
 9083 FORMAT(1X,'**Detailed Error/Message File:   ',A96)
 9084 FORMAT(1X,'**File Created for Event Model:  ',A96)
 9085 FORMAT(1X,'**File for Summary of Results:   ',A96)
99090 FORMAT(/1X,'**Note that special processing requirements apply',
     &    ' for the 24-hour PM2.5 NAAQS - check available guidance.'/,
     &    '   Model will process user-specified ranks of high 24-hour',
     &    ' values averaged across the number of years modeled, and'/,
     &    '   the multi-year average of individual ANNUAL values,',
     &    ' averaged across the number of years modeled.')
99091 FORMAT(/1X,'**Note that special processing requirements apply',
     &  ' for the 1-hour NO2 NAAQS - check available guidance.'/,
     &  '   Model will process user-specified ranks of daily maximum',
     &  ' 1-hour values averaged across the number of years modeled.'/,
     &  '   For annual NO2 NAAQS modeling, the multi-year maximum of',
     &  ' PERIOD values can be simulated using the MULTYEAR keyword.'/,
     &  '   Multi-year PERIOD and 1-hour values should only be done',
     &  ' in a single model run using the MULTYEAR option with a'/,
     &  '   single multi-year meteorological data file using STARTEND',
     &  ' keyword.')
99092 FORMAT(/1X,'**Note that special processing requirements apply',
     &  ' for the 1-hour SO2 NAAQS - check available guidance.'/,
     &  '   Model will process user-specified ranks of daily maximum'/,
     &  ' 1-hour values averaged across the number of years modeled.')


 9099 FORMAT(1X,' ')

      RETURN
      END

      SUBROUTINE PRTO3VALS
C***********************************************************************
C                 PRTO3VALS Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Input Source Data Summary
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, NL, I1, I2, I3, IFR, IDW, INDC, INGRP
      CHARACTER BLDING*3, URB*3, IQUN*12, CAP*3, CQFLG*7
      CHARACTER SEASON(4)*6, MONTHS(12)*9, DAYOFWEEK(3)*8,
     &          DAYOFWEEK7(7)*8, CNPD*5, CAZS*8

C     Variable Initializations
      DATA SEASON /'WINTER','SPRING','SUMMER',' FALL '/
      DATA MONTHS /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     &             'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     &             'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA DAYOFWEEK  /'WEEKDAY ','SATURDAY','SUNDAY  '/
      DATA DAYOFWEEK7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',
     &                 'FRIDAY  ','SATURDAY','SUNDAY  '/
      MODNAM = 'PRTO3VALS'


C     Print User-specified Background Concetrations
      IF (BFLAG .EQ. 'ANNUAL') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9001) OzoneUnits
         WRITE(IOUNIT,9006) O3VARY(1)
      ELSE IF (BFLAG .EQ. 'SEASON') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9002) OzoneUnits
         WRITE(IOUNIT,9004) (SEASON(I1),I1=1,4)
         WRITE(IOUNIT,9006) (O3VARY(I1),I1=1,4)
      ELSE IF (BFLAG .EQ. 'MONTH') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9007) OzoneUnits
         WRITE(IOUNIT,9008)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9010) (O3VARY(I1),I1=1,12)
      ELSE IF (BFLAG .EQ. 'HROFDY') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9011) OzoneUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9014) (I1,O3VARY(I1),I1=1,24)
      ELSE IF (BFLAG .EQ. 'SEASHR') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9018) OzoneUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         DO I1 = 1, 4
            IFR = (I1-1)*24
            WRITE(IOUNIT,9019) SEASON(I1)
            WRITE(IOUNIT,9014) (I2,O3VARY(I2+IFR),I2=1,24)
         END DO
      ELSE IF (BFLAG .EQ. 'HRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99218) OzoneUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK(I1)
            WRITE(IOUNIT,99014) (I3,O3VARY(I3+IDW),I3=1,24)
         END DO
      ELSE IF (BFLAG .EQ. 'HRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79218) OzoneUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK7(I1)
            WRITE(IOUNIT,99014) (I3,O3VARY(I3+IDW),I3=1,24)
         END DO
      ELSE IF (BFLAG .EQ. 'SHRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99018) OzoneUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,O3VARY(I3+IFR+IDW),I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG .EQ. 'SHRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79018) OzoneUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,O3VARY(I3+IFR+IDW),I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG .EQ. 'MHRDOW') THEN
         DO I1 = 1, 3
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99118) OzoneUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,O3VARY(I3+IFR+IDW),I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG .EQ. 'MHRDOW7') THEN
         DO I1 = 1, 7
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79118) OzoneUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,O3VARY(I3+IFR+IDW),I3=1,24)
            END DO
         END DO
      END IF

 9001 FORMAT(/39X,'* ANNUAL (NON-VARYING) OZONE CONCENTRATION',
     &       ' (',A5,') *'/)
 9002 FORMAT(/39X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY ',
     &       'SEASONALLY *'/)
 9004 FORMAT(40X,4(A6,9X)/20X,40('- ')/)
 9006 FORMAT(38X,4(E10.5,5X))
 9007 FORMAT(/41X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY ',
     &        'MONTHLY *'/)
 9008 FORMAT(7X,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',
     &  'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',
     &  'DECEMBER'/)
 9010 FORMAT(5X,12E10.4)
 9011 FORMAT(/28X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY ',
     &       'FOR EACH HOUR OF THE DAY *'/)
 9012 FORMAT(5X,6('HOUR    O3VALS',6X))
99012 FORMAT(2X,8('HOUR   O3VALS',3X))
 9013 FORMAT(1X,65('- ')/)
99013 FORMAT(1X,65('- '))
 9014 FORMAT(4(5X,6(I3,3X,E10.5,4X)/))
99014 FORMAT(2(3X,8(I2,2X,E9.4,3X)/),3X,8(I2,2X,E9.4,3X))
 9015 FORMAT(/25X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY',
     &       ' WITH WIND SPEED *'/)
 9018 FORMAT(/22X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY',
     &       ' SEASONALLY AND DIURNALLY (SEASHR) *'/)
99018 FORMAT(/17X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW) *'/)
79018 FORMAT(/17X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW7) *'/)
99118 FORMAT(/18X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW) *'/)
79118 FORMAT(/18X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW7) *'/)
99218 FORMAT(/19X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW) *'/)
79218 FORMAT(/19X,'* BACKGROUND O3 CONCENTRATIONS (',A5,') WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW7) *'/)
 9019 FORMAT(59X,'SEASON = ',A6)
99019 FORMAT(46X,'SEASON = ',A6,';  DAY OF WEEK = ',A8)
99020 FORMAT(46X,'MONTH = ',A9,';  DAY OF WEEK = ',A8)
99021 FORMAT(46X,'DAY OF WEEK = ',A8)
 9024 FORMAT(26X,6(1X,E12.5))
 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))

      RETURN
      END

      SUBROUTINE PRTSRC
C***********************************************************************
C                 PRTSRC Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        V. Tino
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Print Out The Input Source Data Summary
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        MODIFIED BY D. Strimaitis, SRC (for Wet & Dry DEPOSITION)
C
C        DATE:    November 8, 1993
C
C        MODIFIED:   To remove reference to "STABILITY CATEGORY" and 
C                    correct format statement 9024 for 'WSPEED' 
C                    EMISFACT option, inherited from ISCST3 code
C                    for 'STAR' option.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To include emission factors that vary by
C                    hour-of-day and day-of-week (HRDOW and HRDOW7).
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To modify format for outputting QFLAG from A6 to
C                    A7, and other minor adjustments to formatting.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        MODIFIED:   To incorporate flag for CAPPED and/or HORIZONTAL
C                    stack releases based on BETA-test draft option.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 12/07/06
C
C        MODIFIED:   To remove reference to STAR emission factors,
C                    include identification of capped stacks (POINTCAP),
C                    and emission factors that vary by month, hour-of-day
C                    and day-of-week (MHRDOW and MHRDOW7).
C                    Roger Brode, MACTEC (f/k/a PES), Inc., - 08/25/05
C
C        MODIFIED by YICHENG ZHUANG, SRC to combine version 93188 with
C                 version 93046 - 9/28/93
C
C        MODIFIED BY D. Strimaitis, SRC (for DEPOSITION) - 2/25/93
C
C*       MODIFIED BY PES (for OPENPIT Source) - 7/22/94
C
C*       MODIFIED BY PES to properly handle page breaks in summary
C*                of sources within a source group - 11/19/98
C
C*       MODIFIED BY R. Brode, PES to include additional building
C                 dimensions for PRIME downwash algorithm - 8/9/01
C
C*       MODIFIED BY R. Brode, MACTEC/PES to include identification
C                 of urban and Method 2 sources - 9/29/03
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, NL, I1, I2, I3, IFR, IDW, INDC, INGRP
      CHARACTER BLDING*3, URB*3, IQUN*12, CAP*3, CQFLG*7
      CHARACTER SEASON(4)*6, MONTHS(12)*9, DAYOFWEEK(3)*8,
     &          DAYOFWEEK7(7)*8, CNPD*5, CAZS*8

C     Variable Initializations
      DATA SEASON /'WINTER','SPRING','SUMMER',' FALL '/
      DATA MONTHS /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     &             'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     &             'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA DAYOFWEEK  /'WEEKDAY ','SATURDAY','SUNDAY  '/
      DATA DAYOFWEEK7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',
     &                 'FRIDAY  ','SATURDAY','SUNDAY  '/
      MODNAM = 'PRTSRC'

      IF (ISSTAT(8) .EQ. 0 .AND. ISSTAT(17) .EQ. 0 .AND. 
     &                           ISSTAT(18) .EQ. 0) THEN
C        Write Default Emission Rate Units
         IQUN = ' (GRAMS/SEC)'
      ELSE
         IQUN = '(USER UNITS)'
      END IF

C     Write Out The Point Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I)(1:5) .EQ. 'POINT') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            BLDING = 'NO'
            IF (NSEC .GT. 0) THEN
C ---          Check for building data for this source
               DO J = 1, NSEC
                  IF(ADSBH(J,I).NE.0.0D0 .AND. ADSBW(J,I).NE.0.0D0 
     &                                   .AND. ADSBL(J,I).NE.0.0D0) THEN
c -----------------------------------------------------------------
                     BLDING = 'YES'
                     EXIT
                  END IF
               END DO
            END IF
            IF (SRCTYP(I) .EQ. 'POINTCAP') THEN
               CAP = 'CAP'
            ELSE IF (SRCTYP(I) .EQ. 'POINTHOR') THEN
               CAP = 'HOR'
            ELSE
               CAP = ' NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9046) IQUN
            END IF
            WRITE(IOUNIT,9047) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), ATS(I), AVS(I), 
     &              ADS(I), BLDING, URB, CAP, QFLAG(I)
         END IF
      END DO

C     Write Out The Volume Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'VOLUME') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9074) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,9075) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), ASYINI(I), ASZINI(I),
     &              URB, CQFLG
         END IF
      END DO

C     Write Out The Area Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'AREA') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9076) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,9077) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), AXINIT(I), AYINIT(I),
     &              AANGLE(I), ASZINI(I), URB, CQFLG
         END IF
      END DO

C     Write Out The AREACIRC Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'AREACIRC') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9078) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,9079) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), RADIUS(I),
     &              NVERTS(I), ASZINI(I), URB, CQFLG
         END IF
      END DO

C     Write Out The AREAPOLY Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'AREAPOLY') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9080) IQUN
            END IF
            IF (QFLAG(I) .EQ. 'HOURLY' .AND. L_HRLYSIG(I)) THEN
C              Source uses HOUREMIS option with hourly varying sigmas
               CQFLG = 'HRLYSIG'
            ELSE
               CQFLG = QFLAG(I)
            END IF
            WRITE(IOUNIT,9081) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), NVERTS(I),
     &              ASZINI(I), URB, CQFLG
         END IF
      END DO

C*    Write Out The OpenPit Source Data, If Any
      INDC = 0
      DO I = 1, NUMSRC
         IF (SRCTYP(I) .EQ. 'OPENPIT') THEN
            INDC = INDC + 1
            IF (URBSRC(I) .EQ. 'Y') THEN
               URB = 'YES'
            ELSE
               URB = 'NO'
            END IF
            IF (L_METHOD2(I)) THEN
               WRITE(CNPD,'("METH2")')
            ELSE
               WRITE(CNPD,'(I4,1X)') INPD(I)
            END IF
            IF (L_FLATSRC(I)) THEN
               WRITE(CAZS,'(4X,"FLAT")')
            ELSE
               WRITE(CAZS,'(F8.1)') AZS(I)
            END IF
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9082) IQUN
            END IF
            WRITE(IOUNIT,9083) SRCID(I), CNPD, AQS(I),
     &              AXS(I), AYS(I), CAZS, AHS(I), AXINIT(I), AYINIT(I),
     &              AANGLE(I), AVOLUM(I), URB, QFLAG(I)
         END IF
      END DO

      IF (.NOT. PSDCREDIT) THEN
C        Print The Source Group IDs with Source IDs
         INDC = 0
         DO J = 1, NUMGRP
            INGRP = 0
            DO K = 1, NUMSRC
               IF (IGROUP(K,J) .EQ. 1) THEN
                  INGRP = INGRP + 1
                  WORKID(INGRP) = SRCID(K)
               END IF
            END DO
C ---       Check for BACKGRND "source" being included 
C           in source group
            IF (GRP_BACK(J)) THEN
               INGRP = INGRP + 1
               WORKID(INGRP) = 'BACKGROUND'
            END IF
C           Determine Number of Lines @ 8/Line
            NL = 1 + INT((INGRP-1)/8)
            DO K = 1, NL
               INDC = INDC + 1
               IF (MOD(INDC-1,20) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9058)
               END IF
               IF (K .EQ. 1 .AND. K .EQ. NL) THEN
                  WRITE(IOUNIT,9068) GRPID(J), (WORKID(I),I=1,INGRP)
               ELSE IF (K .EQ. 1 .AND. K .NE. NL) THEN
                  WRITE(IOUNIT,9068) GRPID(J), (WORKID(I),I=1,8*K)
               ELSE IF (K .EQ. NL) THEN
                  WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),INGRP)
               ELSE
                  WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),8*K)
               END IF
            END DO
         END DO
      END IF

C     Print The OLM Source Group IDs with Source IDs
      INDC = 0
      DO J = 1, NUMOLM
         INGRP = 0
         DO K = 1, NUMSRC
            IF (IGRP_OLM(K,J) .EQ. 1) THEN
               INGRP = INGRP + 1
               WORKID(INGRP) = SRCID(K)
            END IF
         END DO
C        Determine Number of Lines @ 8/Line
         NL = 1 + INT((INGRP-1)/8)
         DO K = 1, NL
            INDC = INDC + 1
            IF (MOD(INDC-1,20) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9059)
            END IF
            IF (K .EQ. 1 .AND. K .EQ. NL) THEN
               WRITE(IOUNIT,9068) OLMID(J), (WORKID(I),I=1,INGRP)
            ELSE IF (K .EQ. 1 .AND. K .NE. NL) THEN
               WRITE(IOUNIT,9068) OLMID(J), (WORKID(I),I=1,8*K)
            ELSE IF (K .EQ. NL) THEN
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),INGRP)
            ELSE
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),8*K)
            END IF
         END DO
      END DO

C     Print The PSD Source Group IDs with Source IDs for PSDCREDIT Option
      INDC = 0
      DO J = 1, NUMPSD
         INGRP = 0
         DO K = 1, NUMSRC
            IF (IGRP_PSD(K,J) .EQ. 1) THEN
               INGRP = INGRP + 1
               WORKID(INGRP) = SRCID(K)
            END IF
         END DO
C        Determine Number of Lines @ 8/Line
         NL = 1 + INT((INGRP-1)/8)
         DO K = 1, NL
            INDC = INDC + 1
            IF (MOD(INDC-1,20) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,99059)
            END IF
            IF (K .EQ. 1 .AND. K .EQ. NL) THEN
               WRITE(IOUNIT,9068) PSDID(J), (WORKID(I),I=1,INGRP)
            ELSE IF (K .EQ. 1 .AND. K .NE. NL) THEN
               WRITE(IOUNIT,9068) PSDID(J), (WORKID(I),I=1,8*K)
            ELSE IF (K .EQ. NL) THEN
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),INGRP)
            ELSE
               WRITE(IOUNIT,9067) (WORKID(I),I=1+8*(K-1),8*K)
            END IF
         END DO
      END DO

C     Print out NO2_RATIO Data for OLM and PVMRM Options
      IF (OLM .OR. PVMRM) THEN
         INDC = 0
         DO I = 1, NUMSRC, 4
            INDC = INDC + 1
            IF (MOD(INDC-1,40) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9060)
            END IF
            IF (I+3 .LE. NUMSRC) THEN
               WRITE(IOUNIT,9061) SRCID(I), ANO2_RATIO(I),
     &                            SRCID(I+1), ANO2_RATIO(I+1),
     &                            SRCID(I+2), ANO2_RATIO(I+2),
     &                            SRCID(I+3), ANO2_RATIO(I+3)
            ELSE IF (I+2 .LE. NUMSRC) THEN
               WRITE(IOUNIT,9061) SRCID(I), ANO2_RATIO(I),
     &                            SRCID(I+1), ANO2_RATIO(I+1),
     &                            SRCID(I+2), ANO2_RATIO(I+2)
            ELSE IF (I+1 .LE. NUMSRC) THEN
               WRITE(IOUNIT,9061) SRCID(I), ANO2_RATIO(I),
     &                            SRCID(I+1), ANO2_RATIO(I+1)
            ELSE
               WRITE(IOUNIT,9061) SRCID(I), ANO2_RATIO(I)
            END IF
         END DO
      END IF

C     Print Out Wet or Dry Deposition Information.
      INDC = 0
      DO I = 1, NUMSRC
         NPD = INPD(I)
         IF (NPD .NE. 0 .AND. .NOT.L_METHOD2(I)) THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9049)
            END IF
            WRITE(IOUNIT,9050) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,9051) (APHI(J,I),J=1,NPD)
            WRITE(IOUNIT,9052) (APDIAM(J,I),J=1,NPD)
            WRITE(IOUNIT,9053) (APDENS(J,I),J=1,NPD)
         ELSE IF (NPD .NE. 0 .AND. L_METHOD2(I)) THEN
C           Summarize inputs for Method 2 particle deposition
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9049)
            END IF
            WRITE(IOUNIT,9050)  SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99051) (FINEMASS(I),J=1,NPD)
            WRITE(IOUNIT,9052)  (APDIAM(J,I),J=1,NPD)
            WRITE(IOUNIT,9053)  (APDENS(J,I),J=1,NPD)
         ELSE IF (LWGAS .OR. (LDGAS .AND. .NOT.LUSERVD)) THEN
C           Summarize inputs for gas deposition option
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9049)
            END IF
            WRITE(IOUNIT,9050) SRCID(I), SRCTYP(I)
            IF (LDGAS) THEN
               WRITE(IOUNIT,99090) PDIFF(I)
               WRITE(IOUNIT,99091) PDIFFW(I)
               WRITE(IOUNIT,99093) RCLI(I)
               WRITE(IOUNIT,9094)  HENRY(I)
            END IF

         END IF
      END DO

C     Write Out Direction Specific Bldg. Dimensions, If Present
      INDC = 0
      DO I = 1, NUMSRC
         BLDING = 'NO'
         IF (NSEC .GT. 0) THEN
C ---       Check for building data for this source
            DO J = 1, NSEC
               IF(ADSBH(J,I).NE.0.0D0 .AND. ADSBW(J,I).NE.0.0D0 
     &                                .AND. ADSBL(J,I).NE.0.0D0) THEN
c --------------------------------------------------------------
                  BLDING = 'YES'
                  EXIT
               END IF
            END DO
         END IF
         IF (BLDING .EQ. 'YES') THEN
            INDC = INDC + 1
C           Print Out Direction Specific Bldg. Dimensions
            IF (MOD(INDC-1,4) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9064)
            END IF
            WRITE(IOUNIT,9062) SRCID(I),
     &          (J,DABS(ADSBH(J,I)),ADSBW(J,I),ADSBL(J,I),ADSXADJ(J,I),
     &            ADSYADJ(J,I),J=1,NSEC)
c --------------------------------------------------------------------
         END IF
      END DO

C     Print Source Emission Rate Scalars.
      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SEASON') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,6) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9002)
               WRITE(IOUNIT,9004) (SEASON(I1),I1=1,4)
            END IF
            WRITE(IOUNIT,9005) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9006) (QFACT(I1,I),I1=1,4)
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'MONTH') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,6) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9007)
               WRITE(IOUNIT,9008)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9010) (QFACT(I1,I),I1=1,12)
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'HROFDY') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,5) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9011)
               WRITE(IOUNIT,9012)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9014) (I1,QFACT(I1,I),I1=1,24)
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'WSPEED') THEN
            INDC = INDC + 1
            IF (MOD(INDC-1,3) .EQ. 0) THEN
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,9015)
               WRITE(IOUNIT,9013)
            END IF
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            WRITE(IOUNIT,9025) (J, J=1,6)
            WRITE(IOUNIT,9024) (QFACT(I2,I),I2=1,6)
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SEASHR') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,9018)
            WRITE(IOUNIT,9012)
            WRITE(IOUNIT,9013)
            WRITE(IOUNIT,9009) SRCID(I),SRCTYP(I)
            DO I1 = 1, 4
               IFR = (I1-1)*24
               WRITE(IOUNIT,9019) SEASON(I1)
               WRITE(IOUNIT,9014) (I2,QFACT(I2+IFR,I),I2=1,24)
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'HRDOW') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99218)
            WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            DO I1 = 1, 3
               IDW = (I1-1)*24
               WRITE(IOUNIT,99021) DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,QFACT(I3+IDW,I),I3=1,24)
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'HRDOW7') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79218)
            WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            DO I1 = 1, 7
               IDW = (I1-1)*24
               WRITE(IOUNIT,99021) DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,QFACT(I3+IDW,I),I3=1,24)
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SHRDOW') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99018)
            WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            DO I1 = 1, 3
               IDW = (I1-1)*96
               DO I2 = 1, 4
                  IFR = (I2-1)*24
                  WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK(I1)
                  WRITE(IOUNIT,99014) (I3,QFACT(I3+IFR+IDW,I),I3=1,24)
               END DO
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'SHRDOW7') THEN
            INDC = INDC + 1
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79018)
            WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            DO I1 = 1, 7
               IDW = (I1-1)*96
               DO I2 = 1, 4
                  IFR = (I2-1)*24
                  WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK7(I1)
                  WRITE(IOUNIT,99014) (I3,QFACT(I3+IFR+IDW,I),I3=1,24)
               END DO
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'MHRDOW') THEN
            DO I1 = 1, 3
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,99118)
               WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
               WRITE(IOUNIT,99012)
               WRITE(IOUNIT,99013)
               IDW = (I1-1)*288
               DO I2 = 1, 12
                  IFR = (I2-1)*24
                  WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK(I1)
                  WRITE(IOUNIT,99014) (I3,QFACT(I3+IFR+IDW,I),I3=1,24)
               END DO
            END DO
         END IF
      END DO

      INDC = 0
      DO I = 1, NUMSRC
         IF (QFLAG(I) .EQ. 'MHRDOW7') THEN
            DO I1 = 1, 7
               CALL HEADER(IOUNIT)
               WRITE(IOUNIT,79118)
               WRITE(IOUNIT,99009) SRCID(I), SRCTYP(I)
               WRITE(IOUNIT,99012)
               WRITE(IOUNIT,99013)
               IDW = (I1-1)*288
               DO I2 = 1, 12
                  IFR = (I2-1)*24
                  WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK7(I1)
                  WRITE(IOUNIT,99014) (I3,QFACT(I3+IFR+IDW,I),I3=1,24)
               END DO
            END DO
         END IF
      END DO

 9002 FORMAT(/39X,'* SOURCE EMISSION RATE SCALARS WHICH VARY ',
     &       'SEASONALLY *'/)
 9004 FORMAT(40X,4(A6,9X)/20X,40('- ')/)
 9005 FORMAT(/10X,' SOURCE ID = ',A12,' ;  SOURCE TYPE = ',A8,' :')
 9006 FORMAT(38X,4(E10.5,5X))
 9007 FORMAT(/41X,'* SOURCE EMISSION RATE SCALARS WHICH VARY MONTHLY *',
     &       /)
 9008 FORMAT(7X,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',
     &  'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',
     &  'DECEMBER'/)
 9009 FORMAT(/' SOURCE ID = ',A12,' ; SOURCE TYPE = ',A8,' :')
99009 FORMAT(' SOURCE ID = ',A12,' ; SOURCE TYPE = ',A8,' :')
 9010 FORMAT(5X,12E10.4)
 9011 FORMAT(/28X,'* SOURCE EMISSION RATE SCALARS WHICH VARY FOR EACH',
     &       ' HOUR OF THE DAY *'/)
 9012 FORMAT(5X,6('HOUR    SCALAR',6X))
99012 FORMAT(2X,8('HOUR   SCALAR',3X))
 9013 FORMAT(1X,65('- ')/)
99013 FORMAT(1X,65('- '))
 9014 FORMAT(4(5X,6(I3,3X,E10.5,4X)/))
99014 FORMAT(2(3X,8(I2,2X,E9.4,3X)/),3X,8(I2,2X,E9.4,3X))
 9015 FORMAT(/25X,'* SOURCE EMISSION RATE SCALARS WHICH VARY WITH',
     &       ' WIND SPEED *'/)
 9018 FORMAT(/22X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' SEASONALLY AND DIURNALLY (SEASHR) *'/)
99018 FORMAT(/17X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW) *'/)
79018 FORMAT(/17X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW7) *'/)
99118 FORMAT(/18X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW) *'/)
79118 FORMAT(/18X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW7) *'/)
99218 FORMAT(/19X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW) *'/)
79218 FORMAT(/19X,'* SOURCE EMISSION RATE SCALARS WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW7) *'/)
 9019 FORMAT(59X,'SEASON = ',A6)
99019 FORMAT(46X,'SEASON = ',A6,';  DAY OF WEEK = ',A8)
99020 FORMAT(46X,'MONTH = ',A9,';  DAY OF WEEK = ',A8)
99021 FORMAT(46X,'DAY OF WEEK = ',A8)
 9024 FORMAT(26X,6(1X,E12.5))
 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))
 9046 FORMAT(//50X,'*** POINT SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',20X,'BASE     STACK   STACK',4X,
     & 'STACK     STACK    BLDG   URBAN  CAP/  EMIS RATE',/3X,
     & 'SOURCE',7X,'PART. ',A12,5X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  TEMP.   EXIT VEL. DIAMETER',2X,'EXISTS SOURCE ',
     & 'HOR   SCALAR',/4X,' ID         CATS.              ',
     & 1X,2('(METERS) (METERS) '),'(DEG.K) ',' (M/SEC) ',1X,'(METERS)',
     & 22X,'VARY BY'/65(' -')/)
 9047 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,4F9.2,
     &       4X,A3,5X,A3,2x,A3,2X,A7)
 9049 FORMAT(/48X,'*** SOURCE PARTICULATE/GAS DATA ***'//)
 9050 FORMAT(//8X,'*** SOURCE ID = ',A12,'; SOURCE TYPE = ',A8,' ***')
 9051 FORMAT(/8X,'MASS FRACTION ='/4(:10X,10(:F9.5,', ')/))
99051 FORMAT(/8X,'FINE PARTICLE MASS FRACTION ='/4(:10X,10(:F9.5,', ')
     &       /))
 9052 FORMAT(/8X,'PARTICLE DIAMETER (MICRONS) ='/4(:10X,10(:F9.5,', ')
     &       /))
 9053 FORMAT(/8X,'PARTICLE DENSITY (G/CM**3)  ='/4(:10X,10(:F9.5,', ')
     &       /))
 9058 FORMAT(//43X,'*** SOURCE IDs DEFINING SOURCE GROUPS ***'//
     &       1X,'GROUP ID',49X,'SOURCE IDs'/)
 9059 FORMAT(/41X,'*** SOURCE IDs DEFINING OLM SOURCE GROUPS ***'/
     &        41X,'***        FOR COMBINING PLUMES           ***'//
     &       1X,'OLMGRP ID',49X,'SOURCE IDs'/)
99059 FORMAT(/41X,'*** SOURCE IDs DEFINING PSD SOURCE GROUPS ***'/
     &        41X,'***      FOR PVMRM PSDCREDIT OPTION       ***'//
     &       1X,'PSDGRP ID',49X,'SOURCE IDs'/)
 9060 FORMAT(//39X,'*** IN-STACK NO2 RATIOS FOR OLM/PVMRM OPTIONS ***'//
     &       /1X,4('SOURCE_ID',4X,'NO2_RATIO',6X)/)
 9061 FORMAT(1X,4(A12,2X,F7.3,7X))
 9068 FORMAT(//2X,A8,1X,8(1X,A12,','))
 9067 FORMAT(/11X,8(1X,A12,','))

c --- PRIME --------------------------------------------------
 9062 FORMAT(/' SOURCE ID: ',A12,
     &    /,2('  IFV    BH      BW      BL     XADJ    YADJ',3X)
     &    ,/,18(2(2X,I3,5(F7.1,','),2X)/))
c ------------------------------------------------------------

 9064 FORMAT(/42X,'*** DIRECTION SPECIFIC BUILDING DIMENSIONS ***'/)
 9074 FORMAT(//50X,'*** VOLUME SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',20X,'BASE    RELEASE    INIT.',4X,
     & 'INIT.   URBAN  EMISSION RATE',/3X,
     & 'SOURCE',7X,'PART. ',A12,5X,'X',8X,'Y',6X,'ELEV.   ',
     & 'HEIGHT      SY       SZ     SOURCE  SCALAR VARY',
     & /4X,' ID         CATS.              ',
     & 1X,3('(METERS) (METERS) '),13X,'BY'/61(' -')/)
 9075 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,1X,F8.2,1X,
     &       F8.2,5X,A3,3X,A7)
 9076 FORMAT(//50X,'*** AREA SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',2X,'COORD (SW CORNER)',2X,
     & 'BASE     RELEASE  X-DIM     Y-DIM    ORIENT.',4X,
     & 'INIT.   URBAN  ',
     & 'EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  OF AREA   OF AREA   OF AREA     SZ     SOURCE ',
     & ' SCALAR VARY',/4X,' ID         CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),2('(METERS)',2X),' (DEG.)  (METERS)',
     & 14X,'BY'/66(' -')/)
 9077 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,3(1X,F9.2),1X,F8.2,
     &       5X,A3,3X,A7)
 9078 FORMAT(//48X,'*** AREACIRC SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',4X,'CENTER OF AREA',3X,
     & 'BASE     RELEASE  RADIUS     NUMBER     INIT.',
     &  3X,'URBAN  EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT   OF AREA   OF VERTS.    SZ     SOURCE  SCALAR VARY',
     & /4X,' ID         CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),21X,'(METERS)',14X,'BY'
     & /63(' -')/)
 9079 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,2X,F9.2,4X,I4,4X,
     &       F8.2,5X,A3,3X,A7)
 9080 FORMAT(//48X,'*** AREAPOLY SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',3X,'LOCATION OF AREA',2X,
     & 'BASE     RELEASE  NUMBER      INIT.',3X,'URBAN  EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  OF VERTS.     SZ     SOURCE  SCALAR VARY',
     & /4X,' ID         CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),11X,'(METERS)              BY'
     & /63(' -')/)
 9081 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,4X,I4,5X,F8.2,5X,
     & A3,3X,A7)
 9082 FORMAT(//50X,'*** OPENPIT SOURCE DATA ***'//15X,
     & 'NUMBER EMISSION RATE',2X,'COORD (SW CORNER)',2X,
     & 'BASE     RELEASE  X-DIM     Y-DIM    ORIENT.',4X,
     & 'VOLUME',3X,'URBAN  EMISSION RATE',
     & /3X,'SOURCE',7X,'PART. ',A11,7X,'X',8X,'Y',6X,'ELEV.    ',
     & 'HEIGHT  OF PIT    OF PIT    OF PIT     OF PIT   ',
     & 'SOURCE  SCALAR VARY',
     & /4X,' ID         CATS.   /METER**2)  ',
     & 1X,2('(METERS) (METERS) '),2('(METERS)',2X),' (DEG.) ',3X,
     & '(M**3)               BY'
     & /66(' -')/)
 9083 FORMAT(1X,A12,2X,A5,2X,E11.5,2F10.1,A8,F9.2,3(1X,F9.2),
     &       3X,E10.5,2X,A3,3X,A7)
99090 FORMAT(/10X,'DIFF IN AIR (M**2/SEC)     =',2X,E9.2)
99091 FORMAT(/10X,'DIFF IN WATER (M**2/SEC)   =',2X,E9.2)
99093 FORMAT(/10X,'LEAF LIPID RESIST (SEC/M)  =',2X,E9.2)
 9094 FORMAT(/10X,'HENRY`S LAW COEFFICIENT    =',2X,E9.2)

      RETURN
      END

      SUBROUTINE PRTBKG
C***********************************************************************
C                 PRTBKG Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Summary of BACKGRND Data
C
C        PROGRAMMER: Roger Brode
C
C        DATE:       February 28, 2011
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, NL, I1, I2, I3, IFR, IDW, INDC, INGRP
      CHARACTER BLDING*3, URB*3, IQUN*12, CAP*3, CQFLG*7
      CHARACTER SEASON(4)*6, MONTHS(12)*9, DAYOFWEEK(3)*8,
     &          DAYOFWEEK7(7)*8, CNPD*5, CAZS*8

C     Variable Initializations
      DATA SEASON /'WINTER','SPRING','SUMMER',' FALL '/
      DATA MONTHS /'JANUARY  ','FEBRUARY ','MARCH    ','APRIL    ',
     &             'MAY      ','JUNE     ','JULY     ','AUGUST   ',
     &             'SEPTEMBER','OCTOBER  ','NOVEMBER ','DECEMBER '/
      DATA DAYOFWEEK  /'WEEKDAY ','SATURDAY','SUNDAY  '/
      DATA DAYOFWEEK7 /'MONDAY  ','TUESDAY ','WEDNESDY','THURSDAY',
     &                 'FRIDAY  ','SATURDAY','SUNDAY  '/
      MODNAM = 'PRTBKG'

C     Print User-specified Background Concetrations
      IF (BFLAG .EQ. 'ANNUAL') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9001) BackUnits
         WRITE(IOUNIT,9006) BACKGRND(1)
      ELSE IF (BFLAG .EQ. 'SEASON') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9002) BackUnits
         WRITE(IOUNIT,9004) (SEASON(I1),I1=1,4)
         WRITE(IOUNIT,9006) (BACKGRND(I1),I1=1,4)
      ELSE IF (BFLAG .EQ. 'MONTH') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9007) BackUnits
         WRITE(IOUNIT,9008)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9010) (BACKGRND(I1),I1=1,12)
      ELSE IF (BFLAG .EQ. 'HROFDY') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9011) BackUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         WRITE(IOUNIT,9014) (I1,BACKGRND(I1),I1=1,24)
      ELSE IF (BFLAG .EQ. 'SEASHR') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9018) BackUnits
         WRITE(IOUNIT,9012)
         WRITE(IOUNIT,9013)
         DO I1 = 1, 4
            IFR = (I1-1)*24
            WRITE(IOUNIT,9019) SEASON(I1)
            WRITE(IOUNIT,9014) (I2,BACKGRND(I2+IFR),I2=1,24)
         END DO
      ELSE IF (BFLAG .EQ. 'HRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99218) BackUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK(I1)
            WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IDW),I3=1,24)
         END DO
      ELSE IF (BFLAG .EQ. 'HRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79218) BackUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*24
            WRITE(IOUNIT,99021) DAYOFWEEK7(I1)
            WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IDW),I3=1,24)
         END DO
      ELSE IF (BFLAG .EQ. 'SHRDOW') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,99018) BackUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 3
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IFR+IDW),I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG .EQ. 'SHRDOW7') THEN
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,79018) BackUnits
         WRITE(IOUNIT,99012)
         WRITE(IOUNIT,99013)
         DO I1 = 1, 7
            IDW = (I1-1)*96
            DO I2 = 1, 4
               IFR = (I2-1)*24
               WRITE(IOUNIT,99019) SEASON(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IFR+IDW),I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG .EQ. 'MHRDOW') THEN
         DO I1 = 1, 3
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,99118) BackUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK(I1)
               WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IFR+IDW),I3=1,24)
            END DO
         END DO
      ELSE IF (BFLAG .EQ. 'MHRDOW7') THEN
         DO I1 = 1, 7
            CALL HEADER(IOUNIT)
            WRITE(IOUNIT,79118) BackUnits
            WRITE(IOUNIT,99012)
            WRITE(IOUNIT,99013)
            IDW = (I1-1)*288
            DO I2 = 1, 12
               IFR = (I2-1)*24
               WRITE(IOUNIT,99020) MONTHS(I2), DAYOFWEEK7(I1)
               WRITE(IOUNIT,99014) (I3,BACKGRND(I3+IFR+IDW),I3=1,24)
            END DO
         END DO
      END IF

 9001 FORMAT(/35X,'* ANNUAL (NON-VARYING) BACKGROUND CONCENTRATION',
     &       ' (',A5,') *'/)
 9002 FORMAT(/35X,'* BACKGROUND CONCENTRATIONS WHICH VARY SEASONALLY',
     &       ' (',A5,') *'/)
 9004 FORMAT(40X,4(A6,9X)/20X,40('- ')/)
 9006 FORMAT(38X,4(E10.5,5X))
 9007 FORMAT(/37X,'* BACKGROUND CONCENTRATIONS WHICH VARY MONTHLY',
     &       ' (',A5,') *'/)
 9008 FORMAT(7X,'JANUARY  FEBRUARY   MARCH     APRIL      MAY       ',
     &  'JUNE      JULY     AUGUST   SEPTEMBER  OCTOBER  NOVEMBER  ',
     &  'DECEMBER'/)
 9010 FORMAT(5X,12E10.4)
 9011 FORMAT(/24X,'* BACKGROUND CONCENTRATIONS WHICH VARY FOR EACH',
     &       ' HOUR OF THE DAY (',A5,') *'/)
 9012 FORMAT(5X,6('HOUR    BKGRND',6X))
99012 FORMAT(2X,8('HOUR   BKGRND',3X))
 9013 FORMAT(1X,65('- ')/)
99013 FORMAT(1X,65('- '))
 9014 FORMAT(4(5X,6(I3,3X,E10.5,4X)/))
99014 FORMAT(2(3X,8(I2,2X,E9.4,3X)/),3X,8(I2,2X,E9.4,3X))
 9015 FORMAT(/21X,'* BACKGROUND CONCENTRATIONS WHICH VARY WITH',
     &       ' WIND SPEED (',A5,') *'/)
 9018 FORMAT(/18X,'* BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' SEASONALLY AND DIURNALLY (SEASHR) (',A5,') *'/)
99018 FORMAT(/13X,'* BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW)',
     &       ' (',A5,') *'/)
79018 FORMAT(/13X,'* BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' SEASONALLY, DIURNALLY AND BY DAY OF WEEK (SHRDOW7)',
     &       ' (',A5,') *'/)
99118 FORMAT(/14X,'* BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW)',
     &       ' (',A5,') *'/)
79118 FORMAT(/14X,'* BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' MONTHLY, DIURNALLY AND BY DAY OF WEEK (MHRDOW7)',
     &       ' (',A5,') *'/)
99218 FORMAT(/15X,'* BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW)',
     &       ' (',A5,') *'/)
79218 FORMAT(/15X,'* BACKGROUND CONCENTRATIONS WHICH VARY',
     &       ' DIURNALLY AND BY DAY OF WEEK (HRDOW7)',
     &       ' (',A5,') *'/)
 9019 FORMAT(59X,'SEASON = ',A6)
99019 FORMAT(46X,'SEASON = ',A6,';  DAY OF WEEK = ',A8)
99020 FORMAT(46X,'MONTH = ',A9,';  DAY OF WEEK = ',A8)
99021 FORMAT(46X,'DAY OF WEEK = ',A8)
 9024 FORMAT(26X,6(1X,E12.5))
 9025 FORMAT(/26X,6('   WIND SPEED')/26X,6('   CATEGORY',I2))

      RETURN
      END

      SUBROUTINE PRTREC
C***********************************************************************
C                 PRTREC Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Receptor Network Values
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To remove reference to Boundary
C                    Receptors - 4/1/2004
C
C        MODIFIED:   To Adjust Format Statement 9082 for Boundary
C                    Receptors - 9/29/92
C
C        INPUTS:  Arrays of Source Parameters
C                 Arrays of Receptor Locations
C                 Arrays of Model Results
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, INDZ, NX, NY, INDC
      DOUBLE PRECISION :: YCOVAL, XRMS, YRMS, RANGE, RADIAL
      CHARACTER BUF132*132

C     Variable Initializations
      MODNAM = 'PRTREC'
      BUF132 = ' '
      INDZ   = 0

      DO I = 1, INNET
         CALL HEADER(IOUNIT)
         WRITE(IOUNIT,9034)
         WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
         IF (NTTYP(I) .EQ. 'GRIDCART') THEN
            WRITE(IOUNIT,9038)
         ELSE
            WRITE(IOUNIT,9036) XORIG(I), YORIG(I)
            WRITE(IOUNIT,9039)
         END IF
         WRITE(IOUNIT,9040) (XCOORD(J,I),J=1,NUMXPT(I))
         IF (NTTYP(I) .EQ. 'GRIDCART') THEN
            WRITE(IOUNIT,9041)
         ELSE
            WRITE(IOUNIT,9042)
         END IF
         WRITE(IOUNIT,9040) (YCOORD(J,I),J=1,NUMYPT(I))
         IF (ELEV) THEN
C           Print Terrain Heights for Network
C           Set Number of Columns Per Page, NCPP
            NCPP = 9
C           Set Number of Rows Per Page, NRPP
            NRPP = 40
C           Begin LOOP Through Networks
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
                  WRITE(IOUNIT,9011)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  ELSE
                     DO K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZELEV(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
C           Print Hill Height Scales for Network
C           Set Number of Columns Per Page, NCPP
            NCPP = 9
C           Set Number of Rows Per Page, NRPP
            NRPP = 40
C           Begin LOOP Through Networks
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
                  WRITE(IOUNIT,9012)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZHILL(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZHILL(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  ELSE
                     DO K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZHILL(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZHILL(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
         END IF
         IF (FLGPOL) THEN
C           Print The Receptor Heights Above Ground for This Network
C           Set Number of Columns Per Page, NCPP
            NCPP = 9
C           Set Number of Rows Per Page, NRPP
            NRPP = 40
C           Begin LOOP Through Networks
C           Calculate Number of Pages Per X-Group, NPPX, & Per Y-Group, NPPY
            NPPX = 1 + INT((NUMXPT(I)-1)/NCPP)
            NPPY = 1 + INT((NUMYPT(I)-1)/NRPP)
            DO NX = 1, NPPX
               DO NY = 1, NPPY
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9037) NTID(I), NTTYP(I)
                  WRITE(IOUNIT,9035)
                  IF (NX .EQ. NPPX) THEN
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NUMXPT(I))
                     END IF
                  ELSE
                     IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                        WRITE(IOUNIT,9016)
                        WRITE(IOUNIT,9017) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                        WRITE(IOUNIT,9018)
                        WRITE(IOUNIT,9019) (XCOORD(J,I),J=1+NCPP*(NX-1),
     &                                                    NCPP*NX)
                     END IF
                  END IF
                  WRITE(IOUNIT,9010)
                  IF (NY .EQ. NPPY) THEN
                     DO K = 1+NRPP*(NY-1), NUMYPT(I)
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  ELSE
                     DO K = 1+NRPP*(NY-1), NRPP*NY
                        IF (NTTYP(I) .EQ. 'GRIDCART') THEN
                           INDZ = NETEND(I) - K*NUMXPT(I) + 1
                           YCOVAL = YCOORD(NUMYPT(I)-K+1,I)
                        ELSE IF (NTTYP(I) .EQ. 'GRIDPOLR') THEN
                           INDZ = NETSTA(I) + (K-1)*NUMXPT(I)
                           YCOVAL = YCOORD(K,I)
                        END IF
                        IF (NX .EQ. NPPX) THEN
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NUMXPT(I))
                        ELSE
                           WRITE(IOUNIT,9013) YCOVAL,
     &                   (AZFLAG(INDZ+J-1),J=1+NCPP*(NX-1),NCPP*NX)
                        END IF
                     END DO
                  END IF
               END DO
            END DO
         END IF
      END DO

      IF (IRSTAT(4).NE.0 .OR. IRSTAT(8).NE.0) THEN
C ---    Include EVALCART receptors with DISCCART receptors.
C        Print Out The Coordinates, Height , Hill Height & Flags For 
C        Discrete Cart Receptors

         INDC = 0
         DO I = 1, NUMREC
            IF (RECTYP(I) .EQ. 'DC') THEN
               INDC = INDC + 1
               IF (MOD(INDC-1,90) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9043)
               END IF
               IF (MOD(INDC,2) .NE. 0) THEN
                  WRITE(BUF132(1:65),9045) AXR(I),AYR(I),AZELEV(I),
     &                                     AZHILL(I),AZFLAG(I)
               ELSE
                  WRITE(BUF132(66:130),9045) AXR(I),AYR(I),AZELEV(I),
     &                                       AZHILL(I),AZFLAG(I)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) .NE. 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

      IF (IRSTAT(5) .NE. 0) THEN
C        Print Out The Coordinates, Height & Flags For Discrete Polar Receptors
         INDC = 0
         DO I = 1, NUMREC
            IF (RECTYP(I) .EQ. 'DP') THEN
               INDC = INDC + 1
               XRMS = AXR(I) - AXS(IREF(I))
               YRMS = AYR(I) - AYS(IREF(I))
               RANGE  = DSQRT(XRMS*XRMS + YRMS*YRMS)
               RADIAL = DATAN2(XRMS, YRMS) * RTODEG
               IF(RADIAL .LE. 0.0D0) RADIAL = RADIAL + 360.0D0
               IF (MOD(INDC-1,90) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9044)
               END IF
               IF (MOD(INDC,2) .NE. 0) THEN
                  WRITE(BUF132(1:65),9047) SRCID(IREF(I)),RANGE,RADIAL,
     &                                     AZELEV(I),AZHILL(I),AZFLAG(I)
               ELSE
                  WRITE(BUF132(66:130),9047) SRCID(IREF(I)),RANGE,
     &                                       RADIAL,AZELEV(I),AZHILL(I),
     &                                       AZFLAG(I)
                  WRITE(IOUNIT,9090) BUF132
                  WRITE(BUF132,9095)
               END IF
            END IF
         END DO
         IF (MOD(INDC,2) .NE. 0) THEN
            WRITE(IOUNIT,9090) BUF132
            WRITE(BUF132,9095)
         END IF
      END IF

 9010 FORMAT(66(' -')/)
 9011 FORMAT(/48X,'* ELEVATION HEIGHTS IN METERS *'/)
 9012 FORMAT(/48X,'* HILL HEIGHT SCALES IN METERS *'/)
 9013 FORMAT(2X,F10.2,1X,'|',1X,9(1X,F12.2,:))
 9016 FORMAT(3X,' Y-COORD  |',48X,'X-COORD (METERS)')
 9017 FORMAT(3X,' (METERS) |',1X,9(1X,F12.2,:))
 9018 FORMAT(3X,'DIRECTION |',48X,'DISTANCE (METERS)')
 9019 FORMAT(3X,'(DEGREES) |',1X,9(1X,F12.2,:))
 9035 FORMAT(/44X,'* RECEPTOR FLAGPOLE HEIGHTS IN METERS *'/)
 9034 FORMAT(/40X,'*** GRIDDED RECEPTOR NETWORK SUMMARY ***')
 9036 FORMAT(/42X,'*** ORIGIN FOR POLAR NETWORK ***'/,
     &      32X,'X-ORIG =',F10.2,' ;   Y-ORIG = ',F10.2,'  (METERS)')
 9037 FORMAT(/34X,'*** NETWORK ID: ',A8,' ;  NETWORK TYPE: ',
     &       A8,' ***')
 9038 FORMAT(/42X,'*** X-COORDINATES OF GRID ***'/
     &       52X,'(METERS)'/)
 9039 FORMAT(/42X,'*** DISTANCE RANGES OF NETWORK ***'/
     &       52X,'(METERS)'/)
 9040 FORMAT(100(5X,10(F10.1,',')/))
 9041 FORMAT(/42X,'*** Y-COORDINATES OF GRID *** ',
     &       /52X,'(METERS)'/)
 9042 FORMAT(/42X,'*** DIRECTION RADIALS OF NETWORK *** ',
     &       /52X,'(DEGREES)'/)
 9043 FORMAT(/45X,'*** DISCRETE CARTESIAN RECEPTORS ***',
     &       /43X,'(X-COORD, Y-COORD, ZELEV, ZHILL, ZFLAG)',
     &       /45X,'              (METERS)'/)
 9044 FORMAT(/43X,'      *** DISCRETE POLAR RECEPTORS ***',
     &       /43X,' ORIGIN:    (DIST, DIR, ZELEV, ZHILL, ZFLAG)',
     &       /43X,' SRCID:    (METERS,DEG,METERS,METERS,METERS)'/)
 9045 FORMAT(4X,' (',4(F9.1,', '),F9.1,'); ')
 9047 FORMAT(1X,A12,': (',F9.1,', ',3(F7.1,', '),F7.1,'); ')
 9090 FORMAT(A132)
 9095 FORMAT(132(' '))

      RETURN
      END

      SUBROUTINE CHKREC
C***********************************************************************
C                 CHKREC Module of the AMS/EPA Regulatory Model - AERMOD
c ----------------------------------------------------------------------
c ---    ISC-PRIME     Version 1.0    Level 970812              Modified
c ---        D. Strimaitis
c ---        Earth Tech, Inc.
c            Prepared for EPRI under contract WO3527-01
c ----------------------------------------------------------------------
C
C        PURPOSE: Print Out The Input Met Data Summary and Source Groups
C
C        PROGRAMMER: Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To include check for receptors beyond MAXDIST from
C                    sources, using center of AREA/AREACIRC/AREAPOLY and
C                    OPENPIT sources.  MAXDIST is set to 80km under the
C                    FASTALL and FASTAREA options.
C                    R. W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To account for new area source algorithm, which
C                    allows for receptors located within the area - 7/7/93
C
C        MODIFIED:   To account for OpenPit Source - PES - 7/22/94
C
C        INPUTS:  Source and Receptor Inputs
C
C        OUTPUTS: Listing of Receptors Too Close To Sources
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: INC, INOUT
      DOUBLE PRECISION :: XSRC, YSRC, DIST, XVM(5), YVM(5)

C     Variable Initializations
      MODNAM = 'CHKREC'
      INC = 0
      XSRC = 0.0D0
      YSRC = 0.0D0

C     Begin Source LOOP
      DO ISRC = 1, NUMSRC

C        Set Effective Source Radius Based on Source Type
         IF (SRCTYP(ISRC)(1:5) .EQ. 'POINT') THEN
            XRAD = 0.0D0
            XSRC = AXS(ISRC)
            YSRC = AYS(ISRC)
         ELSE IF (SRCTYP(ISRC) .EQ. 'VOLUME') THEN
            XRAD = 2.15D0 * ASYINI(ISRC)
            XSRC = AXS(ISRC)
            YSRC = AYS(ISRC)
         ELSE IF (SRCTYP(ISRC)(1:4) .EQ. 'AREA') THEN
C           Set XRAD to -1 since no minimum distance for 
C           AREA source types.  Use center coordinates 
C           for comparison to MAXDIST.
            XRAD = -1.0D0
            XSRC = AXCNTR(ISRC)
            YSRC = AYCNTR(ISRC)
         ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C           Set XRAD to -1 since minimum distance for 
C           OPENPIT source is handled separately to 
C           flag receptors within the boundary of the
C           OPENPIT source.  Use center coordinates 
C           for comparison to MAXDIST.
            XRAD   = -1.0D0
            XSRC   = AXCNTR(ISRC)
            YSRC   = AYCNTR(ISRC)
            XVM(1) = AXVERT(1,ISRC)
            XVM(2) = AXVERT(2,ISRC)
            XVM(3) = AXVERT(3,ISRC)
            XVM(4) = AXVERT(4,ISRC)
            XVM(5) = AXVERT(5,ISRC)
            YVM(1) = AYVERT(1,ISRC)
            YVM(2) = AYVERT(2,ISRC)
            YVM(3) = AYVERT(3,ISRC)
            YVM(4) = AYVERT(4,ISRC)
            YVM(5) = AYVERT(5,ISRC)
         END IF

C        Begin Receptor LOOP
         DO IREC = 1, NUMREC

C           Calculate DIST From Source to Receptor
            X = AXR(IREC) - XSRC
            Y = AYR(IREC) - YSRC
            DIST = DSQRT (X*X + Y*Y) - XRAD

            IF (DIST .LT. 0.99D0) THEN
C              Receptor Is Too Close To Source
               INC = INC + 1
               IF (MOD((INC-1), 40) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9002)
               END IF
               WRITE(IOUNIT,9003) SRCID(ISRC), AXR(IREC),
     &                            AYR(IREC), DIST

            ELSE IF (DIST .GT. MAXDIST) THEN
C              Receptor Is Too Far From Source
               INC = INC + 1
               IF (MOD((INC-1), 40) .EQ. 0) THEN
                  CALL HEADER(IOUNIT)
                  WRITE(IOUNIT,9002)
               END IF
               WRITE(IOUNIT,9003) SRCID(ISRC), AXR(IREC),
     &                            AYR(IREC), DIST

            ELSE IF (SRCTYP(ISRC) .EQ. 'OPENPIT') THEN
C              Check for receptors within boundary of an open pit source
               XR = AXR(IREC)
               YR = AYR(IREC)
               CALL PNPOLY(XR,YR,XVM,YVM,5,INOUT)
               IF (INOUT .GT. 0) THEN
C                 Receptor is within boundary
                  INC = INC + 1
                  IF (MOD((INC-1), 40) .EQ. 0) THEN
                     CALL HEADER(IOUNIT)
                     WRITE(IOUNIT,9002)
                  END IF
                  WRITE(IOUNIT,9004) SRCID(ISRC), AXR(IREC),
     &                               AYR(IREC)
               END IF
            END IF

         END DO
C        End Receptor LOOP

      END DO
C     End Source LOOP

 9002 FORMAT(/22X,
     & '* SOURCE-RECEPTOR COMBINATIONS FOR WHICH CALCULATIONS ',
     & 'MAY NOT BE PERFORMED *'/24X,'LESS THAN 1.0 METER;',
c --- PRIME ---------------------------------------------------------
     & ' WITHIN OPENPIT; OR BEYOND 80KM FOR FASTAREA/FASTALL',//
c -------------------------------------------------------------------
     & /30X,'SOURCE',10X,'- - RECEPTOR LOCATION - -',9X,'DISTANCE',
     & /30X,'  ID  ',10X,'XR (METERS)   YR (METERS)',9X,'(METERS)',
     & /28X,31('- ')/)
 9003 FORMAT(29X,A12,3X,F13.1,1X,F13.1,5X,F12.2)
 9004 FORMAT(29X,A12,3X,F13.1,1X,F13.1,5X,'     OPENPIT')

      RETURN
      END

      SUBROUTINE PRTMET(IOUNT)
C***********************************************************************
C                 PRTMET Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: Print Out The Input Met Data Summary and Source Groups
C
C        PROGRAMMER: Jeff Wang, Roger Brode
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   To include output file unit argument to support
C                    output to main 'aermod.out' file and to the
C                    optional SUMMFILE.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   To output 4-digit start year and end year for
C                    Y2K compliance.
C                    R.W. Brode, PES, Inc., 5/12/99
C
C        INPUTS:  Model Options and Keyword Summarys
C
C        OUTPUTS: Printed Model Outputs
C
C        CALLED FROM:   INPSUM
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, IOUNT

C     Variable Initializations
      MODNAM = 'PRTMET'

C     Start New Page and Print The Titles
      CALL HEADER(IOUNT)

C     Print The Meteorology Data Date Array.
      WRITE(IOUNT,9037) (IPROC(I),I = 1, 366)

      IF (ISDATE .NE. 0 .OR. IEDATE .NE. 2147123124) THEN
C        Write Out User-specified Start and End Dates
         WRITE(IOUNT,9038) ISYR, ISMN, ISDY, ISHR,
     &                     IEYR, IEMN, IEDY, IEHR
      END IF

      WRITE(IOUNT,9039)

C     Print the upper bound of the first 5 wind speed categories
      WRITE ( IOUNT, 9001 ) (UCAT(I), I=1,5)

 9001 FORMAT(//34X,'*** UPPER BOUND OF FIRST THROUGH FIFTH WIND SPEED',
     &       ' CATEGORIES ***'/60X,'(METERS/SEC)'//46X,5(F7.2,','))
 9037 FORMAT(/44X,'*** METEOROLOGICAL DAYS SELECTED FOR PROCESSING ***'
     &       /63X,'(1=YES; 0=NO)'//8(11X,5(10I2,2X)/))
 9038 FORMAT(/23X,'METEOROLOGICAL DATA PROCESSED BETWEEN START DATE: ',
     &       I4,1X,3I3,/59X,'AND END DATE: ',I4,1X,3I3)
 9039 FORMAT(/16X,'NOTE:  METEOROLOGICAL DATA ACTUALLY PROCESSED WILL',
     &       ' ALSO DEPEND ON WHAT IS INCLUDED IN THE DATA FILE.'/)

      RETURN
      END

      SUBROUTINE RSINIT
C***********************************************************************
C                 RSINIT Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Initialize Results Variables for Restart
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Use local variable, IRSDATE, to read 10-digit start 
C                    date from re-start file.  Include checks on restart
C                    date being ealier than user-specified start date 
C                    on STARTEND keyword, with fatal error message for
C                    INITFILE restarts and warning for MULTYEAR restarts.
C                    Also check for overlapping periods for MULTYEAR 
C                    applications with start date from STARTEND keyword 
C                    being earlier than start date from the MULTYEAR 
C                    re-start file.  This condition results in a fatal 
C                    error message.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Added arrays associated with post-1997 PM10
C                    processing.
C                    R.W. Brode, PES, Inc.,  5/12/99
C
C        MODIFIED:   Changed parameter for specifying the number of
C                    high annual/period averages from NVAL to NHIANN.
C                    R.W. Brode, PES, Inc.,  4/3/98
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized Variables
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

      INTEGER :: I, J, K, L, M
      INTEGER :: JSYR, JSMN, JSDY, JSHR
      INTEGER :: IRSDATE

      INTEGER IDYMAX(12)

C     Variable Initializations
      DATA IDYMAX/31,29,31,30,31,30,31,31,30,31,30,31/
      MODNAM = 'RSINIT'

C --- Read "start date" from restart file, IRSDATE, based on last
C     hour processed in SAVEFILE; IRSDATE is full 10-digit value
C     based on 4-digit year
      READ(IRSUNT,ERR=99,END=999) IRSDATE, NTOTHRS
      
C --- Adjust IRSDATE by adding 1 hour since IRSDATE will be used as the
C     first hour of data to process in the restarted run for INITFILE
      JSYR = IRSDATE/1000000
      JSMN = (IRSDATE/10000) - (IRSDATE/1000000)*100
      JSDY = (IRSDATE/100) - (IRSDATE/10000)*100
      JSHR = IRSDATE - (IRSDATE/100)*100
      IF (JSHR .LT. 24) THEN
         IRSDATE = IRSDATE + 1
      ELSE
         JSHR = 1
         IF (JSDY .LT. IDYMAX(JSMN)) THEN
            JSDY = JSDY + 1
            JSMN = JSMN
            JSYR = JSYR
         ELSE
            JSDY = 1
            IF (JSMN .LT. 12) THEN
               JSMN = JSMN + 1
               JSYR = JSYR
            ELSE
               JSMN = 1
               JSYR = JSYR + 1
            END IF
         END IF
         IRSDATE = JSYR*1000000 + JSMN*10000 + JSDY*100 + JSHR
      END IF

C --- Compare "start date" from restart file, IRSDATE, to "start date"
C     from STARTEND keyword (IMSTAT(6)=1), ISDATE.
C     If IRSDATE < ISDATE, then issue fatal error message for INITFILE
C     restarts and warning message for MULTYEAR restarts;
C     IF IRSDATE > ISDATE for MULTYEAR restarts, then issue fatal error
C     message since this implies overlapping data periods.
      IF (.NOT.MULTYR .AND. IMSTAT(6) .EQ. 1 .AND. 
     &                                       IRSDATE .LT. ISDATE) THEN
C        Re-start date is less than start date based on STARTEND keyword
         CALL ERRHDL(PATH,MODNAM,'E','484','STARTEND')
         RUNERR = .TRUE.
         GO TO 1000
      ELSE IF (MULTYR .AND. IMSTAT(6) .EQ. 1 .AND. 
     &                                       IRSDATE .LT. ISDATE) THEN
         CALL ERRHDL(PATH,MODNAM,'W','485','STARTEND')

      ELSE IF (MULTYR .AND. IMSTAT(6) .EQ. 1 .AND. 
     &                                       IRSDATE .GT. ISDATE) THEN
         WRITE(DUMMY,'(I8.8)') IRSDATE - (IRSDATE/100000000)*100000000
         CALL ERRHDL(PATH,MODNAM,'E','486',DUMMY)
         RUNERR = .TRUE.
         GO TO 1000

      ELSE
C        Assign IRSDATE to ISDATE as start date for data processing
         ISDATE = IRSDATE
      END IF
      
      READ(IRSUNT,ERR=99,END=999) NHIVAL, NMXVAL, NUMREC, NUMGRP,
     &                            NUMAVE, NUMTYP

      IF (NHIVAL .GT. 0) THEN
         READ(IRSUNT,ERR=99,END=999) (((((HIVALU(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((((NHIDAT(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((((HCLMSG(I,J,K,L,M),I=1,NUMREC),
     &                   J=1,NHIVAL),K=1,NUMGRP),L=1,NUMAVE),M=1,NUMTYP)

C ---    Include arrays associated with multi-year processing of high
C        ranked values for 24-hr PM2.5, 1-hr NO2, and 1-hr SO2 NAAQS
         IF (PM25AVE .OR. NO2AVE .OR. SO2AVE) THEN
            READ(IRSUNT,ERR=99,END=999) NUMYRS
            READ(IRSUNT,ERR=99,END=999) (((SUMHNH(I,J,K),I=1,NUMREC),
     &                                       J=1,NUMGRP),K=1,NHIVAL)
            READ(IRSUNT,ERR=99,END=999) (((HIMXDLY(I,J,K),I=1,NUMREC),
     &                                       J=1,NUMGRP),K=1,NHIVAL)
            READ(IRSUNT,ERR=99,END=999) (((NHIDATMXD(I,J,K),I=1,NUMREC),
     &                                       J=1,NUMGRP),K=1,NHIVAL)
            READ(IRSUNT,ERR=99,END=999) ((((HIMXDLY_BYYR(I,J,K,L),
     &                   I=1,NUMREC),J=1,NUMGRP),K=1,NHIVAL),L=1,NUMYRS)
            READ(IRSUNT,ERR=99,END=999) ((((NHIDATMXD_BYYR(I,J,K,L),
     &                   I=1,NUMREC),J=1,NUMGRP),K=1,NHIVAL),L=1,NUMYRS)
         END IF

      END IF

      IF (NMXVAL .GT. 0) THEN
         READ(IRSUNT,ERR=99,END=999) ((((RMXVAL(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MXDATE(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MXLOCA(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((((MCLMSG(I,J,K,L),I=1,NMXVAL),
     &                               J=1,NUMGRP),K=1,NUMAVE),L=1,NUMTYP)
      END IF

      IF (SEASONHR) THEN
C        Initialize the SEASON by HOUR-OF-DAY Arrays
         READ(IRSUNT,ERR=99,END=999) (((((SHVALS(I,J,K,L,M),I=1,NUMREC),
     &                           J=1,NUMGRP),K=1,4),L=1,24),M=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) ((NSEAHR(I,J),I=1,4),J=1,24)
         READ(IRSUNT,ERR=99,END=999) ((NSEACM(I,J),I=1,4),J=1,24)
      END IF

      IF (PERIOD) THEN
         READ(IRSUNT,ERR=99,END=999) IANHRS, IANCLM, IANMSG, NUMYRS
         READ(IRSUNT,ERR=99,END=999) (((ANNVAL(I,J,K),I=1,NUMREC),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
         IF (MULTYR) THEN
C           Reinitialize the ANNVAL(NUMREC,NUMGRP,NUMTYP) Array and Annual Counters
            ANNVAL = 0.0D0
            IANHRS = 0
            IANCLM = 0
            IANMSG = 0
C           Read the Maximum Annual Values
            READ(IRSUNT,ERR=99,END=999) (((AMXVAL(I,J,K),I=1,NHIANN),
     &                                       J=1,NUMGRP),K=1,NUMTYP)
            READ(IRSUNT,ERR=99,END=999) (((IMXLOC(I,J,K),I=1,NHIANN),
     &                                       J=1,NUMGRP),K=1,NUMTYP)
         END IF
      ELSE IF (ANNUAL) THEN
         READ(IRSUNT,ERR=99,END=999) IANHRS, IANCLM, IANMSG, NUMYRS
         READ(IRSUNT,ERR=99,END=999) (((ANNVAL(I,J,K),I=1,NUMREC),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
         READ(IRSUNT,ERR=99,END=999) (((SUMANN(I,J,K),I=1,NUMREC),
     &                                    J=1,NUMGRP),K=1,NUMTYP)
      END IF

      GO TO 1000

C     WRITE Error Message:  Error Reading INITFILE
 99   DUMMY = 'INITFILE'
      CALL ERRHDL(PATH,MODNAM,'E','510',DUMMY)
      RUNERR = .TRUE.
      GO TO 1000

C     WRITE Error Message:  End of File Reached for INITFILE
 999  DUMMY = 'INITFILE'
      CALL ERRHDL(PATH,MODNAM,'E','580',DUMMY)
      RUNERR = .TRUE.

 1000 RETURN
      END

      SUBROUTINE RESINI
C***********************************************************************
C                 RESINI Module of the AMS/EPA Regulatory Model - AERMOD
C
C        PURPOSE: To Initialize Results Variables With Zeroes
C
C        PROGRAMMER: Roger Brode, Jeff Wang
C
C        DATE:    March 2, 1992
C
C        MODIFIED:   Replaced DO-loops with array assignment statements,
C                    and checked for allocation status of allocatable
C                    arrays.
C                    R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009
C
C        MODIFIED:   Added results arrays for post-1997 PM10 processing
C                    option.  Also replaced labeled DO loop terminators
C                    with unlabeled END DO statements.
C                    R.W. Brode, PES, Inc.,  11/19/98
C
C        MODIFIED:   Changed parameter for specifying the number of
C                    high annual/period averages from NVAL to NHIANN.
C                    R.W. Brode, PES, Inc.,  4/3/98
C
C        INPUTS:  None
C
C        OUTPUTS: Initialized Variables
C
C        CALLED FROM:   MAIN
C***********************************************************************

C     Variable Declarations
      USE MAIN1
      IMPLICIT NONE
      CHARACTER MODNAM*12

C     Variable Initializations
      MODNAM = 'RESINI'

C     Initialize the Results Arrays
      NUMHRS(:) = 0
      NUMCLM(:) = 0
      NUMMSG(:) = 0
      IF (ALLOCATED(HRVAL))  HRVAL  = 0.0D0
      IF (ALLOCATED(AVEVAL)) AVEVAL = 0.0D0
      IF (ALLOCATED(HIVALU)) HIVALU = 0.0D0
      IF (ALLOCATED(NHIDAT)) NHIDAT = 0
      IF (ALLOCATED(HCLMSG)) HCLMSG = ' '
      IF (ALLOCATED(HMAX))   HMAX   = 0.0D0
      IF (ALLOCATED(HMDATE)) HMDATE = 0
      IF (ALLOCATED(HMLOC))  HMLOC  = 0
      IF (ALLOCATED(HMCLM))  HMCLM  = ' '
      IF (ALLOCATED(RMXVAL)) RMXVAL = 0.0D0
      IF (ALLOCATED(MXDATE)) MXDATE = 0
      IF (ALLOCATED(MXLOCA)) MXLOCA = 0
      IF (ALLOCATED(MCLMSG)) MCLMSG = ' '
      
      IANHRS = 0
      IANCLM = 0
      IANMSG = 0

C     The following were added as part of implementing the SCIM option
      NSKIPTOT = 0

C     Initialize results arrays for ANNUAL/PERIOD processing; if allocated
      IF (ALLOCATED(ANNVAL))  ANNVAL = 0.0D0
      IF (ALLOCATED(SUMANN))  SUMANN = 0.0D0
      IF (ALLOCATED(AMXVAL))  AMXVAL = 0.0D0
      IF (ALLOCATED(IMXLOC))  IMXLOC = 0

C     Initialize results array for PM-2.5 processing; if allocated
      IF (ALLOCATED(SUMHNH))  SUMHNH  = 0.0D0
      IF (ALLOCATED(MXPMVAL)) MXPMVAL = 0.0D0
      IF (ALLOCATED(MXPMLOC)) MXPMLOC = 0

C     Initialize results array for SEASONHR option; check for allocated first
      IF (ALLOCATED(SHVALS)) SHVALS = 0.0D0
C     Initialize results array for MAXDAILY option; check for allocated first
      IF (ALLOCATED(MXDVAL))  MXDVAL  = 0.0D0
      IF (ALLOCATED(HIMXDLY)) HIMXDLY = 0.0D0
      IF (ALLOCATED(HIMXDLY_BYYR)) HIMXDLY_BYYR = 0.0D0
      IF (ALLOCATED(IMXDHR))  IMXDHR  = 0
      IF (ALLOCATED(NHIDATMXD)) NHIDATMXD = 0
      IF (ALLOCATED(NHIDATMXD_BYYR)) NHIDATMXD_BYYR = 0
      
      NSEAHR = 0
      NSEACM = 0

      RETURN
      END
