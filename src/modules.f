      MODULE MAIN1
C***********************************************************************
C     MAIN1
C     AERMOD Model Data - Parameter, Variable and Array Declarations
C                         Global Data for All Modules
C
C***********************************************************************

      IMPLICIT NONE

c ----------------------------------------------------------------------
c --- ISC-PRIME Version 1.0
c
c     ISCST3 modified to accept additional building data required by
c     the PRIME building downwash modules
c
c     Changes are denoted in comment fields
c
c     Prepared by    Earth Tech, Inc
c     Prepared for   EPRI under contract WO3527-01
c ----------------------------------------------------------------------

C***********************************************************************
C     User Specified Model Parameters for Array Dimensions
C***********************************************************************

C --- Most array limits for data storage are now allocated at runtime.
      INTEGER, PARAMETER :: NWSCAT= 6, NKST= 6, NHR= 24,
     &                      NPAIR= 100, NHIANN= 10, 
     &                      NMXPM= 10, MXPLVL=50, MXGLVL=87

      INTEGER :: NYEARS

C**   NWSCAT = Number of Wind Speed Categories
C**   NKST   = Number of Stability Categories
C**   NHR    = Number of Hours in Met Data Loop
C**   NPAIR  = Number of Pairs of TXCONC and IDCONC for TOXXFILE Output
C**   NHIANN = Number of high period/annual averages to include in the
C**                   summary page of the output file (formerly controlled
C**                   by NVAL)
C**   NMXPM  = Number of high average High-N-High 24-hour PM2.5 averages or
C**                   High-N-High 1-hour NO2 or SO2 averages to include in the
C**                   summary table for PM-2.5/NO2/SO2 processing across years
C**   MXPLVL = Maximum number of levels in the observed profile file
C**   MXGLVL = Maximum number of levels in the gridded profiles (0 - 4000 m)
C**   NYEARS = Number of Years allowed for multi-year analyses for PM2.5,
C**                   1-hour NO2 and 1-hour SO2 standards, which are averaged
C**                   over the number of years modeled; a default value of 5
C**                   years is assumed, but the user can override the default
C**                   using the ME NUMYEARS keyword; e.g., setting NYEARS = 1
C**                   on the ME NUMYEARS keyword for 1 year of site-specific
C**                   met data will significantly reduce the memory requirements
C**                   for the MAXDCONT option.

C** The following array limits are set dynamically at runtime:
C**   NSRC   = Max Number of Sources
C**   NREC   = Max Number of Receptors
C**   NGRP   = Max Number of Source Groups
C**   NAVE   = Max Number of Short Term Averaging Periods
C**   NVAL   = Max Number of High Values by Receptor (RECTABLE Keyword)
C**   NTYP   = Max Number of Output Types per Run (CONC, DEPOS, DDEP and WDEP)
C**   NMAX   = Max Number of Overall Maximum Values (MAXTABLE Keyword)
C**   NSEC   = Number of Sectors for Building Dimensions
C**   NQF    = Number of Variable Emission Rate Factors Per Source
C**   NBF    = Number of Temporally Varying Background Concentrations
C**   NO3F   = Number of Temporally Varying Ozone Concentrations
C**   NPDMAX = Max Number of Particle Diameter Categories Per Source
C**   NVMAX  = Max Number of Vertices for AREA/AREACIRC/AREAPOLY and/or 
C**            OPENPIT Sources
C**   IXM    = Max Number of X-coord (Distance) Values Per Receptor Network
C**   IYM    = Max Number of Y-coord (Direction) Values Per Receptor Network
C**   NNET   = Max Number of Cartesian and/or Polar Receptor Networks
C**   NARC   = Maximum number of Receptor Groupings ('ARCs') for EVALCART
C**   NURB   = Maximum number of Urban Areas


C***********************************************************************
C     Programmer Specified Model Parameters
C***********************************************************************

      INTEGER, PARAMETER :: IFMAX=150, IKN=91, ISTRG=512, ILEN_FLD=200,
     &                      IERRN=246
C*#

C**   IFMAX  = Max Number of Fields Per Runstream Record
C**   IKN    = Number of Keywords
C**   ISTRG  = Max Length of Runstream Image Record
C**   ILEN_FLD = Max Length of Runstream Input Fields.  Also used 
C**              to specify length of input filenames and formats.
C**   IERRN  = Number of Error/Warning/Informational Messages


C***********************************************************************
C     Model Constants Specified as Parameters
C***********************************************************************

      DOUBLE PRECISION, PARAMETER :: 
     &                    G = 9.80616D0,       VONKAR = 0.4D0,
     &                    GOVRCP = 0.00977D0,  DCTODK = 273.16D0,
     &                    BETA1  = 0.6D0,      BETA2  = 0.4D0,
     &                    AT1PT2 = 1.2D0,      UMINGR = 0.01D0, 
     &                    GSIGV  = 0.073864D0, EFOLDH = 0.44D0,
     &                    SVUMIN = 0.05D0,     SVMIN  = 0.2D0, 
     &                    SWMIN  = 0.02D0,     XVAL   = 0.0D0, 
     &                    SPTGMN = 0.002D0,    BSUBC  = 0.5D0,
     &                    SZCOEF = 2.15D0,     ALPHAR = 1.4D0,
     &                    LAMDAY = 2.3D0,      ASUBE  = 0.1D0,
     &                    REFPOP = 2.0D+6,     DELTRUR= 12.0D0,
     &                    RGAS   = 8.3145D0

C**   Set concentration unit conversion factors for NO2, SO2, CO, and 03
C**   for use with OZONEVAL, OZONEFIL, O3VALUES, and BACKGRND keywords;
C**   factors defined by pollutant for PPB-to-UG/M3 and PPM-to-UG/M3,
C**   based on reference temperature (25 C) and pressure (1013.25 mb).
C**   Note that factors for NO2 and SO2 are PPB/(UG/M3) and PPM/(UG/M3),
C**   and factors for CO and O3 are for (UG/M3)/PPB and (UG/M3)/PPM.
      DOUBLE PRECISION, PARAMETER ::  
     &                    NO2_PPB = 0.5319D0, NO2_PPM = 0.5319D-3,
     &                    SO2_PPB = 0.3823D0, SO2_PPM = 0.3823D-3,
     &                    CO_PPB  = 1.144D0,  CO_PPM  = 1.144D3,
     &                    O3_PPB  = 1.960D0,  O3_PPM  = 1.960D3

           
      DOUBLE PRECISION :: PI, TWOPI, RTOFPI, SRT2PI, RTOF2, RTPIBY2, 
     &                    DTORAD, RTODEG, 
     &                    THIRD

C**   PI     = PI               ! Initialized in sub.VARINI as 4.0D0*DATAN(1.0D0)
C**   TWOPI  = 2.*PI            ! Initialized in sub.VARINI
C**   RTOFPI = SQRT(PI)         ! Initialized in sub.VARINI
C**   SRT2PI = SQRT(2.*PI)      ! Initialized in sub.VARINI
C**   RTOF2  = SQRT(2.)         ! Initialized in sub.VARINI
C**   RTPIBY2= SQRT(PI/2.)      ! Initialized in sub.VARINI
C**   DTORAD = Degrees to Radians Conversion   ! Initialized in sub.VARINI as PI/180.0D0
C**   RTODEG = Radians to Degrees Conversion   ! Initialized in sub.VARINI as 180.0D0/PI
C**   THIRD  = 1.0/3.0          ! Initialized in sub.VARINI

C**   G      = Acceleration Due to Gravity (m/s**2)
C**   VONKAR = von Karman constant
C**   GOVRCP = Gravity divided by specific heat at constant pressure
C**   DCTODK = Degrees Celsius to kelvin conversion factor
C**   BETA1  = Coeff. in the calculation of 'direct' plume rise
C**   BETA2  = Coeff. in the calculation of buoyancy-induced dispersion
C**   AT1PT2 = The fraction of the mixed layer and above the mixed
C              layer through which a variable changes its value
C**   UMINGR = Minimum value for a gridded wind speed
C**   GSIGV  = Constant used in converting sigma_A to sigma_V
C**   EFOLDH = Constant in computation of dTHETA/dZ in stable atmosphere
C**   SVUMIN = Minimum value applied to Sigma_V / U when calculating
C**            Sigma_Y.
C**   SVMIN  = Minimum value applied to measured Sigma_V values
C**   SWMIN  = Minimum value applied to measured Sigma_W values
C**   XVAL   = Gradient in the miXing layer (unstable atmosphere)
C**   SPTGMN = Minimum vert. potential temp. gradient for stable atmosphere
C**   BSUBC  = Constant used in computing sigma_Z for surface layer releases
C**   SZCOEF = Coefficient of sigma-z to define the plume half-width
C**   ALPHAR = Parameter used in January 18, 1995 indirect source MCA
C**   LAMDAY = Parameter used in January 18, 1995 indirect source MCA
C**   ASUBE  = Parameter used in January 18, 1995 indirect source MCA
C**   REFPOP = Reference population for urban option (4/1/96 MCA)
C**   DELTRUR= Surface cooling in the rural area (4/1/96 MCA)
C**   RGAS   = ideal gas law constant = 8.3145 Pa-m^3/mol-K
C**

C**   NUMSYEFF = Number of effective sigma-y's to use with the 
C**              FASTALL non-DFAULT option; 
C**              receptors more than NUMSYEFF*SYEFF off the plume
C**              centerline are skipped to optimize runtime
      DOUBLE PRECISION, PARAMETER :: NUMSYEFF = 4.0D0

C**   MAXDIST = Maximum transport distance for calculation;
C**             set to 80km for FASTALL or FASTAREA options;
C**             this was formerly associated with the TOXICS
C**             option, which is now obsolete.
C**             Set to 1.0D20 for applications w/o FASTALL or FASTAREA
      DOUBLE PRECISION :: MAXDIST


C***********************************************************************
C     Common Block for Input/Output File Units (Initialized in BLOCK DATA)
C***********************************************************************

      INTEGER :: INUNIT, IOUNIT, MFUNIT, MPUNIT, IERUNT, IERWRT,
     &           IDPUNT, IDPUN2, IRSUNT, IEVUNT, ITEVUT, IHREMI,
     &           IBGUNT, INCUNT, DBGUNT, DBMUNT, ICVUNT, IO3UNT,
     &           ISUMUNT, GDEPDBG, PDEPDBG, PVMDBG

C**   These input/output file units are initialized below in a DATA statement
C**   INUNIT = Input Runstream File Unit (Initialized to 7)
C**   IOUNIT = Main Printed Output File Unit (Initialized to 8)
C**   MFUNIT = Input Surface Met Data File Unit (Initialized to 19)
C**   MPUNIT = Input Profile Met Data File Unit (Initialized to 21)
C**   IERUNT = Temporary Error/Message File Unit (Initialized to 10)
C**   IERWRT = Permanent Detailed Error/Message File Unit (Init. to 11)
C**   IDPUNT = Main SAVEFILE Unit for Re-start Option (Init. to 12)
C**   IDPUN2 = Secondary SAVEFILE Unit for Re-start Option (Init. to 14)
C**   IRSUNT = INITFILE Unit for Re-start Option (Initialized to 15)
C**   IEVUNT = Event File Unit for Use With EVENT Model Option (Init. to 17)
C**   ITEVUT = Temporary Event File Used to Store High Value Events for
C**            Summary Tables and for EVENTFIL Option (Initialized to 18)
C**   IBGUNT = Hourly Background Concentration File for BACKGRND Option (Init. to 13)
C**   IHREMI = Hourly Emission Parameters File Unit (Init. to 16)
C**   INCUNT = INCLUDED File Unit (Initialized to 20)
C**   DBGUNT = Debug Output File for Calculations (Init. to 24)
C**   DBMUNT = Debug Output File for Meteorology Profiles (Init. to 25)
C**   ICVUNT = Cavity Concentration Output File (Initialized to 26)
C**   ISUMUNT = Summary File under SUMMFILE Option (Init. to 27)
C**   IO3UNT = Hourly Ozone Data File for PVMRM and OLM Options (Init. to 28)
C**   GDEPDBG = Debug Output File for Gas Deposition Velocities (Init. to 29)
C**   PDEPDBG = Debug Output File for Particle Deposition Velocities (Init. to. 30)
C**   PVMDBG  = Debug Output File for PVMRM Option (Init. to. 9)
C**   The following file units are declared below:
C**   ISUNIT = Surface Meteorology File for SCIM'd data (Initialized to 22)
C**   IPUNIT = Profile Meteorology File for SCIM'd data (Initialized to 23)

C***********************************************************************
C     This is The Global Variable Definition Block for Runstream Data
C***********************************************************************

      LOGICAL BLINE, INFLD, MARK, ECHO

      CHARACTER PATH*2, PPATH*2, KEYWRD*8, PKEYWD*8, KEYWD*8, KTYPE*5,
     &          RUNST*1

      CHARACTER (LEN=ILEN_FLD) :: FIELD, INPFIL, OUTFIL, INCFIL
      CHARACTER (LEN=ISTRG)    :: RUNST1

      INTEGER ::  LOCB(IFMAX), LOCE(IFMAX), IFC, IDC1, IPNUM, IPPNUM
      DIMENSION   FIELD(IFMAX), KEYWD(IKN), RUNST(ISTRG)


C***********************************************************************
C     This is The Global Variable Definition Block for Error Handling
C***********************************************************************

      LOGICAL FATAL, ISTART, IFINIS, RECERR, ERRLST, EOF, ALLOC_ERR
      LOGICAL L_SkipMessages
      
      REAL    :: STORE                ! Estimate of memory storage requirement

      CHARACTER ERRMSG*50, ERRCOD*3, VERSN*6
      CHARACTER (LEN=6) :: C_METVER         ! Character string for met version
      CHARACTER (LEN=ILEN_FLD) :: MSGFIL

      DIMENSION  ERRMSG(IERRN), ERRCOD(IERRN)
      INTEGER :: ILINE, IQLINE, IBLINE, IOLINE, IERROR, IFTL, IWRN,
     &           INFO, ICLM, IMSG, NFATAL, NWARN, IPAGE, IPGSUM
C --- Met data array indices for use with MAXDCONT option
      INTEGER :: IHR_NDX, IYR_NDX
      DOUBLE PRECISION :: EXPLIM
      INTEGER :: ICSTAT(50), ISSTAT(50), IRSTAT(50), IMSTAT(50),
     &           IOSTAT(50), IESTAT(50)
      INTEGER :: INCSET, IXYSET, IEVSET, IHLSET, IFGSET


C***********************************************************************
C     This is The Global Variable Definition Block for COntrol Pathway
C***********************************************************************

      LOGICAL DFAULT, CONC, DEPOS, DDEP, WDEP, RURAL, URBAN, GRDRIS,
     &        NOSTD, NOBID, CLMPRO, MSGPRO, PERIOD, ANNUAL, MONTH,
     &        FLAT, ELEV, FLATSRCS, FLGPOL, RUN, EVENTS, RSTSAV, 
     &        RSTINP, DAYTAB, MXFILE, PPFILE, PLFILE, ANPOST, ANPLOT, 
     &        STATOK, MULTYR, TXFILE, RKFILE, SEASONHR, 
     &        MXDAILY, MXDAILY_BYYR, L_MAXDCONT,
     &        DDPLETE, WDPLETE, DRYDPLT, WETDPLT, NODRYDPLT, NOWETDPLT,
     &        FSTCMP, EVONLY, SOCONT, DETAIL, NEWMET, 
     &        ARDPLETE, DEBUG, METEOR, PM25AVE, NO2AVE, SO2AVE, 
     &        NOCHKD, NOWARN, PVMRMDBG, DEPOSDBG,
     &        L_WARNCHKD, SCIM, SCIMHR, 
     &        FASTAREA, FASTALL, L_NonDFAULT, 
     &        SCREEN, URBSTAB, PRM_FSTREC, ROMBERG,
     &        PVMRM, PSDCREDIT, OLM, O3FILE, L_MULTURB, 
     &        L_PRESET_URBAN, L_UrbanTransition, L_URBAN_ALL,
     &        BETA, MAXDWARN
C*#

      LOGICAL :: L_EFFSIGY
      
      CHARACTER (LEN=ILEN_FLD) :: TITLE1, TITLE2
      CHARACTER RUNDAT*8, RUNTIM*8
      CHARACTER EVPARM*6, CHRAVE*5,
     &          CHIDEP*4, SOELEV*6, REELEV*6, TGELEV*6, OUTTYP*5

      CHARACTER (LEN=ILEN_FLD) :: SAVFIL, SAVFL2, INIFIL, EVFILE,
     &                            DBGFIL, DBMFIL, DBPVFIL,URBNAM, 
     &                            OZONFL, O3FILUNITS, O3VALUNITS, 
     &                            O3FORM, OzoneUnits

      DOUBLE PRECISION ::  O3CONC, O3BACK, NO2Equil, NO2Stack, 
     &                     EV_O3CONC(NHR)
      LOGICAL ::  O3MISS

      INTEGER ::  NHIVAL, NMXVAL, NDUMP, NHIMXDLY

      INTEGER ::  NSRC, NREC, NGRP, NAVE, NVAL, NTYP, NMAX,
     &            NSEC, NQF, NBF, NO3F, NPDMAX, NNET, IXM, IYM,
     &            NEVE, NUMEVE, IEVENT, NARC, NOLM, NURB, NPSD 

      INTEGER ::  NUMCONT     ! Number of contributing sources for PVMRM
      
      INTEGER, ALLOCATABLE :: KAVE(:)

      LOGICAL, ALLOCATABLE :: EVAL(:)

      ALLOCATABLE ::  CHRAVE(:), CHIDEP(:,:), OUTTYP(:), URBNAM(:)
      CHARACTER (LEN=9) :: MODOPS(20)



C***********************************************************************
C     This is The Global Variable Definition Block for SOurce Pathway
C***********************************************************************

      CHARACTER SRCID*12, SRCTYP*8, SOPCRD*1, SOGAS*1, URBSRC*1, 
     &          GRPID*8, EMILBL*40, OUTLBL*40, POLLUT*8, 
     &          QFLAG*8, BFLAG*8, O3FLAG*8, PERLBL*40, OLMID*8, URBID*8,
     &          PSDID*8 

      CHARACTER (LEN=ILEN_FLD) :: HRFILE, BKGRND_File, BGFORM, BackUnits
C*#
      CHARACTER PREVSRCID*12
      CHARACTER PREVGRPID*8

      LOGICAL LDPART, LWPART, LDGAS, LWGAS, L_BACKGRND, L_O3VALUES
      LOGICAL L_HourlyBackgrnd, L_MissBackgrnd
      
      LOGICAL, ALLOCATABLE :: GRP_BACK(:)

      DOUBLE PRECISION :: BGCONC, EV_BGCONC(NHR)

      DOUBLE PRECISION, ALLOCATABLE ::  AXS(:), AYS(:), AZS(:), AQS(:),
     &                        AHS(:), ATS(:), AVS(:), ADS(:),
     &                        ASYINI(:), ASZINI(:), ANO2_RATIO(:),
     &                        ADSFACT(:)
      DOUBLE PRECISION, ALLOCATABLE :: AAQS(:,:,:), AAHS(:,:,:), 
     &                        AATS(:,:,:), AAVS(:,:,:), 
     &                        AASYINI(:,:,:), AASZINI(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE ::  ADSBH(:,:), ADSBW(:,:),
     &                        ADSBL(:,:), ADSXADJ(:,:), ADSYADJ(:,:)

      INTEGER, ALLOCATABLE :: INPD(:), NDXSTK(:)
      
      DOUBLE PRECISION, ALLOCATABLE ::  QFACT(:,:), BACKGRND(:), 
     &                                  O3VARY(:)
      DOUBLE PRECISION :: EMICON, HAFLIF, DECOEF, VAIRMS, ZRDEP, VDPHOR
      DOUBLE PRECISION, ALLOCATABLE :: EMIFAC(:), APDIAM(:,:), 
     &                                 APHI(:,:), APDENS(:,:),
     &                                 AVGRAV(:,:), ATSTOP(:,:)

C*--- Variables for hourly emissions 
      DOUBLE PRECISION ::  HRQS, HRTS, HRVS, HRHS, HRSY, HRSZ
      DOUBLE PRECISION, ALLOCATABLE ::  EV_HRQS(:,:), EV_HRTS(:,:), 
     &                                  EV_HRVS(:,:), EV_HRHS(:,:),
     &                                  EV_HRSY(:,:), EV_HRSZ(:,:)

      INTEGER ::  FULLHRQ, FULLHRB
C*----
C*#
      CHARACTER :: PSDSRCTYP*2
      INTEGER, ALLOCATABLE :: IGROUP(:,:), IGRP_OLM(:,:), IGRP_PSD(:,:),
     &                        IURBGRP(:,:)
      ALLOCATABLE ::  SRCID(:), SRCTYP(:), SOPCRD(:),
     &                SOGAS(:), URBSRC(:),
     &                GRPID(:), QFLAG(:), EMILBL(:),
     &                OUTLBL(:),
     &                PERLBL(:), OLMID(:), PSDID(:), URBID(:)
      LOGICAL, ALLOCATABLE :: L_OLMGRP(:), L_PSDGRP(:)
      LOGICAL, ALLOCATABLE :: L_HRLYSIG(:), L_FLATSRC(:),
     &                        L_WakeMessage(:)

      ALLOCATABLE :: PSDSRCTYP(:)


C**   NDXSTK   Index of the gridded height immediately below stack top
C**   FULLHRQ  Date/Time Stamp for Hourly Emissions Data
C**   FULLHRB  Date/Time Stamp for Hourly Background Data
C**   HRFILE   Hourly Emissions Data File Name
C*#

C***********************************************************************
C     This is The Global Variable Definition Block for the New Area
C     Source Algorithm - 7/7/93
C
C*    Added XVERT,YVERT - Jayant Hardikar, PES, 7/20/94
C***********************************************************************

C**   NVMAX and NVMAX2 are now dynamically allocated at runtime for
C**   AREAPOLY and AREACIRC sources.  If AREACIRC sources are included, 
C**   then the maximum number of vertices needed for AREACIRC sources
C**   is based on the maximum number specified by the user using the
C**   optional NVERTS parameter + 4.  The default number of vertices 
C**   for an AREACIRC source is still set at 20 vertices, but that 
C**   value is only used to determine array storage if the the input 
C**   file includes an AREACIRC source without the NVERTS parameter.
C**   If AREAPOLY sources are included, then NVMAX is set to the maximum 
C**   number vertices specified for an AREAPOLY source + 8 (but not 
C**   less than the number needed for AREACIRC sources).  The '+ 8' 
C**   allows for the additional number of sides on an AREAPOLY source that
C**   may be needed to define the portion of the source being integrated,
C**   depending on the source/receptor/wind direction geometry.  This 
C**   allows for some complexity in the shape of an AREAPOLY source, but 
C**   very complex shapes may result in runtime errors (E406) and should 
C**   be avoided. 
C**   If no AREACIRC or AREAPOLY sources are included, but rectangular
C**   AREA sources and/or OPENPIT sources are included, then the value
C**   of NVMAX is set to 8 (4 sides plus 4).

C**   NVMAX2= NVMAX * 2

      LOGICAL LSEG

      INTEGER :: IVERT, NVERT, NSEGS,
     &           NVMAX, NVMAX2, NPIT, NPNT, NVOL, 
     &           NAREA, NPOLY, NVTEMP, NCIRC, 
     &           NPTEMP
      DOUBLE PRECISION, ALLOCATABLE :: UVERT(:), VVERT(:), VNVERT(:),
     &                                 WVERT(:), UASEGS(:), UBSEGS(:),
     &                                 XVERT(:), YVERT(:)
      DOUBLE PRECISION, ALLOCATABLE :: SPA(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AXINIT(:), AYINIT(:), AANGLE(:),
     &                                 AXVERT(:,:), AYVERT(:,:),
     &                                 RADIUS(:), AXCNTR(:), AYCNTR(:)
      INTEGER, ALLOCATABLE :: NVERTS(:)



C***********************************************************************
C     This is The Global Variable Definition Block for the New OPENPIT
C     Source Algorithm - 7/19/94
C***********************************************************************

      DOUBLE PRECISION, PARAMETER   :: ALPHA = 0.029D0
      DOUBLE PRECISION, ALLOCATABLE :: AALPHA(:), APDEFF(:), AVOLUM(:)
      DOUBLE PRECISION, ALLOCATABLE :: EFRAC(:), QPART(:)
      DOUBLE PRECISION :: PALPHA, THETA, PDEFF, PDREL, PITFRA, QEFF
      DOUBLE PRECISION :: PITLEN, PITWID, PITL, EMIHGT, XEFF, YEFF


C*    ALPHA     = Proportionality Constant for determining Escape Fraction
C*    AALPHA    = Array of Angles of Long Pit Dimension w.r.t.
C*                North for Each Source
C*    PALPHA    = Angle of Long Pit Dimension w.r.t. North for
C*                the Current Source
C*    THETA     = Wind Direction angle w.r.t Long Axis
C*                of the Pit
C*    APDEFF    = Array of Effective Pit Depths for Each Source
C*    PDEFF     = Effective Pit Depths for Current Source
C*    PDREL     = Relative Pit Depth
C*    AVOLUM    = Array of Volume of the OPENPIT Sources
C*    EFRAC     = Array of Escape Fractions
C*    QPART     = Array of Adjusted Emission Rates
C*    PITFRA    = Fractional Size of the Effective Pit Area
C*    PITLEN    = Length of the Pit
C*    PITWID    = Width of the Pit
C*    PITL      = Along-Wind Length of the Pit
C*    EMIHGT    = Height of Emissions Above Base of Pit
C*    XEFF      = X-dimension of Effective Pit
C*    YEFF      = Y-dimension of Effective Pit



c----------------------------------------------------------------------
c --- COMMON BLOCK /DRYGAS/ -- Dry deposition parameters        CALPUFF
c----------------------------------------------------------------------
c
      DOUBLE PRECISION, ALLOCATABLE :: pdiff(:),pdiffw(:),rmolwt(:),
     &                                 alphas(:),react(:),henry(:),
     &                                 rcli(:),finemass(:), scf(:)
     
      LOGICAL, ALLOCATABLE :: L_METHOD2(:)
      
      INTEGER :: ISEAS_GD(12), ILAND_GD(36), NCLOUD
      
      DOUBLE PRECISION :: rm, rcut, qsw, xlai, vdepg, uservd, zsubp, 
     &                    delta_z, FO, FSEAS2, FSEAS5, fracsat, 
     &                    liqcont, denom, xnu

      DOUBLE PRECISION :: Wold, Wnew, f2, EsTa

      CHARACTER (LEN = 40) ::  REFSPE

      LOGICAL  LUSERVD
C
C     REFSPE      - Reference Species (Default is SO2)
c
c --- COMMON BLOCK /DRYGAS/ Variables:
c       PDIFF(NSRC) - real    - Molecular diffusivity (m**2/s)
c                               of each pollutant.
c                               SEE NOTE #1
c      PDIFFW(NSRC) - real    - Molecular diffusivity in water (m**2/s)
c                               of each pollutant.
c                               SEE NOTE #1
c      RMOLWT(NSRC) - real    - Molecular weight of pollutant (g/mol)
c                               of each pollutant.
c      ALPHAS(NSRC) - real    - Solubility enhancement factor due
c                               to the aqueous phase reactivity of
c                               the pollutant.
c       REACT(NSRC) - real    - Reactivity factor for each
c                               pollutant.
c                RM - real    - Mesophyll resistance (s/m)
c                               SEE NOTE #2
c       HENRY(NSRC) - real    - Henry's law constant (ratio of
c                               gas to aqueous phase concentration
c                               of the pollutant).
c              RCUT - real    - Cuticle resistance (s/m).
c
c  NOTE #1: Input units of this variable are cm**2/s.  Conversion to m**2/s
c           is made internally in the SETUP phase.
c
c  NOTE #2: Input units of s/cm are converted to s/m in the SETUP phase.



C***********************************************************************
C     This is The Global Variable Definition Block for REceptor Pathway
C***********************************************************************

      LOGICAL ISTA, IEND, NEWID

      CHARACTER NETID*8, NETIDT*8, PNETID*8, NTID*8, NTTYP*8,
     &          RECTYP*2, PXSOID*12, PESOID*12, ARCID*8

      DOUBLE PRECISION, ALLOCATABLE ::  AXR(:), AYR(:), AZELEV(:),
     &                                  AZFLAG(:), AZHILL(:)
      INTEGER, ALLOCATABLE :: IREF(:), NDXARC(:)
      ALLOCATABLE ::  NETID(:), RECTYP(:), NTID(:),
     &                NTTYP(:), ARCID(:)
      INTEGER ::      ICOUNT, JCOUNT, IZE, IZH, IZF, IRZE, IRZH, IRZF,
     &                IRXR, IRYR, IRHZ, INNET
      DOUBLE PRECISION ::  XINT, YINT
      DOUBLE PRECISION, ALLOCATABLE ::  XCOORD(:,:), YCOORD(:,:),
     &                                  XORIG(:), YORIG(:)
      INTEGER, ALLOCATABLE :: NETSTA(:), NETEND(:),
     &                        NUMXPT(:), NUMYPT(:)

C**  AZHILL Hill Height Associated with the Receptor
C**  HCRIT  Critical dividing streamline associated with the receptor


C***********************************************************************
C     This is The Global Variable Definition Block for MEteorology Pathway
C***********************************************************************

      CHARACTER SFNAME*40, UANAME*40, ONNAME*40, ALAT*10, ALON*10

      CHARACTER (LEN=ILEN_FLD) :: METINP, SCIM_SFCFIL, SCIM_PROFIL,
     &                            PROINP
      CHARACTER (LEN=ILEN_FLD) :: METFRM, PROFRM
      
      LOGICAL SCIMOUT
      
C**** Logical flags for met data version, L_OldMetVer is used to flag
C     an outdated met version date in the surface file header record;
C     L_NewMetData is used to flag cases where surface file header shows
C     current met version date, but surface file lacks additional fields
C     introduced with version 11059 for the wind data source/adjustment.
      LOGICAL :: L_OldMetVer, L_NewMetData

      INTEGER ::      ISDATE, IEDATE, ISYR, ISMN, ISDY, ISHR, IEYR,
     &                IEMN, IEDY, IEHR, IPROC(366),
     &                ISYEAR, IUYEAR, IOYEAR,
     &                IDSURF, IDUAIR, IDSITE, ISJDAY, IEJDAY,
     &                NDAYS, INCRST,
     &                ISTRT_CENT, ISTRT_WIND
C        RWB/MJ - allow for SCIM option - May, 1998.
     &               ,NREGSTART, NREGINT, IFIRSTHR, ISUNIT, IPUNIT
     &               ,NSKIPTOT, IMETMSG

      DOUBLE PRECISION :: UCAT(5), ROTANG,
     &                    VIRTPNT_URB(NKST), VIRTPNT_RUR(NKST), VP_FACT
      DOUBLE PRECISION :: SFX, SFY, UAX, UAY, ONX, ONY
   

C***********************************************************************
C     This is The Global Variable Definition Block for METEXT
C***********************************************************************

      LOGICAL CLMHR, MSGHR, UNSTAB, NEUTRL, STABLE,
     &        RUNERR, PFLERR, ENDMON, METHDR,
     &        HOURLY, L_DayOfWeekOpts

      LOGICAL, ALLOCATABLE :: L_MorningTrans(:), AL_MorningTrans(:,:,:),
     &                        ACLMHR(:,:), AMSGHR(:,:),
     &                        ASTABLE(:,:), AUNSTAB(:,:),
     &                        AURBSTAB(:,:)
      
      INTEGER ::  KSTMSG
      INTEGER ::  IHOUR, IYEAR, IMONTH, IDAY, KURDAT, JDAY, ISEAS,
     &            KHOUR, KYEAR, KMONTH, KDAY, KURPFL, NTOTHRS,
     &            IPHOUR, IPYEAR, IPDATE, IPCODE, KST,
     &            IYR, IDAY_OF_WEEK, IDAY_OF_WEEK7, NPLVLS, NTGLVL,
     &            IFLAG(MXPLVL)
      INTEGER ::  JDAY_PREV
      INTEGER ::  FULLDATE
      DOUBLE PRECISION ::  SFCHF, USTAR, WSTAR, VPTGZI, ZICONV, ZIMECH,
     &                     OBULEN, SFCZ0, BOWEN, ALBEDO, UREF, WDREF,
     &                     UREFHT, TA, TREFHT, ZI, AFV,
     &                     BVF, BVPRIM, XLAT, TSIGN, ZIRUR,
     &                     PRATE, PREC1, PREC2, TOTAL_PRECIP,
     &                     UREF10, RURUSTR, RUROBULEN, RH, SFCP

      DOUBLE PRECISION, ALLOCATABLE :: URBPOP(:), URBZ0(:), ZIURB(:), 
     &                     URBWSTR(:),
     &                     URBUSTR(:), URBOBULEN(:)

      INTEGER, ALLOCATABLE :: IKST(:,:), IAPCODE(:,:), NACLOUD(:,:)
      
      DOUBLE PRECISION, ALLOCATABLE :: APRATE(:,:), AQSW(:,:),ARH(:,:), 
     &                                 ASFCP(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: ASFCHF(:,:), AUREF(:,:), 
     &                                 AUREFHT(:,:), ATA(:,:), 
     &                                 ATREFHT(:,:), AWDREF(:,:), 
     &                                 AUSTAR(:,:), AWSTAR(:,:),
     &                                 AZICONV(:,:), AZIMECH(:,:), 
     &                                 AOBULEN(:,:), AVPTGZI(:,:),
     &                                 ASFCZ0(:,:), ABOWEN(:,:),
     &                                 AALBEDO(:,:), AWNEW(:,:), 
     &                                 AWOLD(:,:), AESTA(:,:), 
     &                                 AF2(:,:), APREC1(:,:), 
     &                                 APREC2(:,:),
     &                                 AO3CONC(:,:), ABGCONC(:,:)

      INTEGER :: ISTRHOUR, ISTRDY, ISTRMN, 
     &           IENDHOUR, IENDDY, IENDMN, NUMYRS, NREMAIN, NDX4ZI

      INTEGER, ALLOCATABLE :: ANDX4ZI(:,:)

      DOUBLE PRECISION :: PFLHT(MXPLVL), PFLWD(MXPLVL), PFLWS(MXPLVL),
     &                    PFLTA(MXPLVL), PFLSA(MXPLVL), PFLSW(MXPLVL),
     &                    PFLSV(MXPLVL), PFLTG(MXPLVL), PFLTGZ(MXPLVL)
     
      DOUBLE PRECISION, ALLOCATABLE :: APFLHT(:,:,:), APFLWD(:,:,:),
     &                                 APFLWS(:,:,:), APFLTA(:,:,:),
     &                                 APFLSA(:,:,:), APFLSW(:,:,:),
     &                                 APFLSV(:,:,:), APFLTG(:,:,:),
     &                                 APFLTGZ(:,:,:)

      INTEGER, ALLOCATABLE :: AIFLAG(:,:,:)
      
      INTEGER, ALLOCATABLE :: ANPLVLS(:,:), ANTGLVL(:,:)
      
      DOUBLE PRECISION :: GRIDHT(MXGLVL), GRIDWD(MXGLVL), 
     &                    GRIDWS(MXGLVL), GRIDSW(MXGLVL), 
     &                    GRIDSV(MXGLVL), GRIDTG(MXGLVL),
     &                    GRIDPT(MXGLVL),
C---  Add density profile for PRIME
     &                    GRIDRHO(MXGLVL),
C---  Add tubulence dissipation rate (epsilon) profile for PVMRM
     &                    GRIDEPS(MXGLVL)
      DOUBLE PRECISION :: GRDSWR(MXGLVL), GRDSVR(MXGLVL), 
     &                    GRDTGR(MXGLVL),
     &                    GRDPTR(MXGLVL)

      DOUBLE PRECISION, ALLOCATABLE :: GRDSWU(:,:), GRDSVU(:,:), 
     &                                 GRDTGU(:,:), GRDPTU(:,:)

      DOUBLE PRECISION, ALLOCATABLE :: 
     &                    AGRIDHT(:,:,:), AGRIDWD(:,:,:), 
     &                    AGRIDWS(:,:,:), AGRIDSW(:,:,:), 
     &                    AGRIDSV(:,:,:), AGRIDTG(:,:,:),
     &                    AGRIDPT(:,:,:),
C---  Add density profile for PRIME
     &                    AGRIDRHO(:,:,:),
C---  Add tubulence dissipation rate (epsilon) profile for PVMRM
     &                    AGRIDEPS(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE :: AGRDSWR(:,:,:), AGRDSVR(:,:,:), 
     &                                 AGRDTGR(:,:,:), AGRDPTR(:,:,:),
     &                                 AUATZI(:,:),  ASVATZI(:,:),
     &                                 ASWATZI(:,:), APTATZI(:,:),
     &                                 AUAVG(:,:),  ASVAVG(:,:),
     &                                 ASWAVG(:,:), APTAVG(:,:)

      DOUBLE PRECISION, ALLOCATABLE :: AGRDSWU(:,:,:,:), 
     &                                 AGRDSVU(:,:,:,:),
     &                                 AGRDTGU(:,:,:,:), 
     &                                 AGRDPTU(:,:,:,:),
     &                                 AZIURB(:,:,:),
     &                                 AURBWSTR(:,:,:),
     &                                 AURBUSTR(:,:,:),
     &                                 AURBOBULEN(:,:,:),
     &                                 ARURUSTR(:,:), 
     &                                 ARUROBULEN(:,:)

      DOUBLE PRECISION :: TG4PFL, TG4XTR,
     &                    THSTAR, SVAVG, SWAVG, UAVG,
     &                    SVATZI, SWATZI, UATZI,
     &                    PTATZI, UATHE, SVATHE, SWATHE,
     &                    UAVH3, SVAVH3, SWAVH3, SWRMAX

C**   BVF    = Brunt-Vaisala frequency
C**   BVPRIM = 0.7*BVF
C**   SFCHF  = Surface heat flux (W/sq m)
C**   USTAR  = Surface friction velocity (m/s)
C**   WSTAR  = Convective scaling velocity (m/s)
C**   VPTGZI = Vertical potential temperature gradient from ZI to ZI+500
C**            (degrees/m)
C**   ZICONV = Hourly convective mixing height estimated by AERMET (m)
C**   ZIMECH = Hourly mechanical mixing height estimated by AERMET (m)
C**   OBULEN = Monin-Obukhov length (m)
C**   SFCZ0  = Surface roughness length (m)
C**   BOWEN  = Bowen ratio = sensible heat flux/latent heat flux
C**   ALBEDO = Albedo at the earth's surface (nondimensional)
C**   UREF   = Reference height wind speed (m/s)
C**   WDREF  = Reference height wind direction (degrees from north)
C**   UREFHT = Reference height for winds (m) (first nonmissing level
C**            of wind speed AND direction above 7.0*SFCZ0)
C**   TA     = Ambient temperature at a reference height (kelvin)
C**   TREFHT = Reference height for temperature (m) (first nonmissing
C**            level of temperature)
C**   ZI     = The mixing height used by AERMOD after any manipulation
C**            and massaging (m)
C**   NPLVLS = Number of levels in the observed hourly profile data
C**   NTGLVL = Number of levels of observed potential temperature gradient
C**   IFLAG  = Top of profile flag: 1 = top level, 0 = level below top
C**   PFLHT  = Profile height above local ground level (m)
C**   PFLWD  = Profile wind direction (degrees from north)
C**   PFLWS  = Profile wind speed (m/s)
C**   PFLTA  = Profile ambient temperature (kelvins)
C**   PFLSA  = Profile sigma_A (degrees)
C**   PFLSW  = Profile sigma_W (m/s)
C**   PFLSV  = Profile sigma_V (m/s), computed from sigma_A and wind speed
C**   PFLTG  = Profile of Vertical Potential Temperature Gradient (kelvin/m)
C**   PFLTGZ = Profile of VPTG heights (midpoint of interval) (m)
C**   GRIDHT = Gridded height (m)
C**   GRIDWD = Gridded wind direction (degrees from north)
C**   GRIDWS = Gridded wind speed (m/s)
C**   GRIDSW = Gridded sigma_W (m/s)
C**   GRIDSV = Gridded sigma_V (m/s)
C**   GRIDTG = Gridded vertical potential temperature gradient (deg/m)
C**   GRIDPT = Gridded potential temperature profile
C**   GRIDRHO= Gridded density profile
C**   GRIDEPS= Gridded tubulence dissipation rate (epsilon) profile for PVMRM
C**   TG4PFL = Potential temperature gradient at 2.0 meters
C**   TG4XTR = Potential temperature gradient at 100.0 meters
C**   XLAT   = Station latitude, decimal degrees
C**   TSIGN  = Sign used for turning of wind: 1.0 for northern hemis.
C**                                          -1.0 for southern hemis.
C**   NDX4ZI = Index of gridded height immediately below ZI
C**   SVAVG  = Average sigma_V from the surface to ZI (m/s)
C**   SWAVG  = Average sigma_W from the surface to ZI (m/s)
C**   UAVG   = Average wind speed from the surface to ZI (m/s)
C**   SVATZI = sigma_V at ZI (m/s)
C**   SWATZI = sigma_W at ZI (m/s)
C**   UATZI  = Wind speed at ZI (m)
C**   PTATZI = Potential temperature at ZI (kelvin)
C**   SVATHE = Average sigma_V from the surface to HS for HS > ZI (m/s)
C**   SWATHE = Average sigma_W from the surface to HS for HS > ZI (m/s)
C**   UATHE  = Average wind speed from the surface to HS for HS > ZI (m/s)
C**   SVAVH3 = Average sigma_V from the surface to HE3 for penetrated plume
C**   SWAVH3 = Average sigma_W from the surface to HE3 for penetrated plume
C**   UAVH3  = Average wind speed from the surface to HE3 for penetrated plume
C**   SWRMAX = Residual vertical turbulence, average of measured sigma-w
C**            above ZI or 0.02 * UATZI


C***********************************************************************
C     This is The Global Variable Definition Block for Calculation
C***********************************************************************

      LOGICAL CALCS, WAKE
      LOGICAL SURFAC

      DOUBLE PRECISION PHID1, PHID2, PHIN1, PHIN2

      INTEGER :: IREC,   ISRC,   IGRP,   IAVE,   ITYP,  ISET,
     &           NUMREC, NUMSRC, NUMGRP, NUMAVE, NUMARC, NUMTYP,
     &           NUMCAP, NUMHOR, NUMFLAT, IBKGRD, IO3SET,
     &           ICYEAR, NURBSRC, NUMURB, NPD, IFVSEC,
     &           IUCAT, IOLM, NUMOLM, IPSD, NUMPSD, IURB
      DOUBLE PRECISION :: XS, YS, ZS, QS, HS, DS, VS, TS, SYINIT,
     &                    SZINIT, XINIT, YINIT, ANGLE, XCNTR, YCNTR,
     &                    DSFACT, DSBH, DSBW,
c --- PRIME Modification -------------------------------------------
     &                    DSBL, XADJ, YADJ, B_SUBS, B_SUBL, RSCALE,
c ------------------------------------------------------------------
     &                    D, VD, E, WDRAD, WDSIN, WDCOS, ZBASE

      DOUBLE PRECISION, ALLOCATABLE :: PDIAM(:), PHI(:), PDENS(:),
     &                                 VGRAV(:), TSTOP(:), SCHMIDT(:),
     &                                 VDEP(:), WQCOR(:), DQCOR(:),
     &                                 PSCVRT(:), WASHOUT(:), ECOLL(:),
     &                                 AWDSIN(:), AWDCOS(:)
      DOUBLE PRECISION :: WQCORG, GSCVRT, DQCORG, WASHOUTG, VSETL
      DOUBLE PRECISION :: XR, YR, X, Y, ZELEV, ZFLAG, ZR, ZEFF, DISTR,
     &                    ZHILL, HCRIT, ZRT, XDIST
      DOUBLE PRECISION :: HE, HSP, HEFLAT, HTER, HEMWAK, HEDHH, ZB, ZM,
     &                    HED1, HED2, HEN1, HEN2, HE3, HPEN, HED1M,
     &                    HED2M, HEN1M, HEN2M, HE3M, HSBL, QSUBN, QSUB3,
     &                    XY, XZ, SBID, FM, FB, DTDZ, DHF, DHFAER, DHP,
     &                    DHP1,DHP2, DHP3, DELT, DHPB, DHPM, XF, XMAX, 
     &                    XFM, XFB, XRAD, WPB, DHCRIT, HTEFF, CENTER,
     &                    Z4GAMMA, XTR4GAMMA
      DOUBLE PRECISION :: HESETL, HE3SETL, HV
      DOUBLE PRECISION :: US, SVS, SWS, TGS, TYS, PTS, UP, WDIR, DA,
     &                    ZLY, ZLB, RINIT, CB, CM, QTK, PPF, PSUBS, FHC,
     &                    SY, SYB, SYN, SY3, SZ, SZUPR, SYAMB, SZAMB,
     &                    SZAS, SZAD1, SZAD2, SZAN1, SZAN2, SYAN, SZA3,
     &                    SZB, SZBD, SZBN, SZ3, SZD1, SZD2, SZN1, SZN2,
     &                    SZEFF, SZSURF, SYA3, SYB3, SZB3, VSY3, VSIGY,
     &                    VSIGZ, VSYN, VSZD1,VSZD2, VSZN1, VSZN2, VSZ3,
     &                    SZD1M, SZD2M, SZN1M, SZN2M, SZ3M, U3, SV3, 
     &                    SW3, TGP
      DOUBLE PRECISION :: FSUBY, FSUBYD, FSUBYN, FSUBY3
      DOUBLE PRECISION :: FSUBZ, FSUBZD, FSUBZN, FSUBZ3, 
     &                    PHEE, FOPT, CWRAP, CLIFT, XMDBG,
     &                    CWRAPC, CLIFTC, FSUBYC, FSBY3C
      DOUBLE PRECISION :: UEFF, SVEFF, SWEFF, TGEFF,
     &                    UEFFD, SVEFFD, SWEFFD,
     &                    UEFFN, SVEFFN, SWEFFN,
     &                    UEFF3, SVEFF3, SWEFF3, TGEFF3,
     &                    EPSEFF, EPSEFFD, EPSEFF3,
     &                    XMIXED, XFINAL, ZMIDMX
      DOUBLE PRECISION :: SKEW, R, ALPHPD, BETAPD, ASUB1, ASUB2,
     &                    BSUB1, BSUB2, LAMDA1, LAMDA2
      DOUBLE PRECISION :: CHIW, CHIDW, CHINW, CHI3W,
     &                    CHIL, CHIDL, CHINL, CHI3L
      DOUBLE PRECISION :: GAMFACT

C**   AZSAVG = Average stack base elevation (m)
C**   ZBASE  = Base elevation used for potential temperature profile (m MSL)
C**   US     = Wind speed at stack height (m/s)
C**   UP     = Stack top wind speed for plume rise computations
C**   WDIR   = Stack top wind direction used for plume transport
C**   SVS    = sigma_V at stack height (m/s)
C**   SWS    = sigma_W at stack height (m/s)
C**   TGS    = Potential temperature gradient at stack top
C**   PTS    = Stack top potential temperature for plume rise
C**   xxEFF  = "effective" value for parameter xx
C**   FHC    = Function of Plume material above HCRIT
c**   PHEE   = "PHI" Term : Fraction of Plume Below Hcrit
C**   FSUBY  = Fy Term (Horizontal Gaussian term)
C**   FSUBYN = Fy Term (Horizontal Gaussian term) for the
C**            Indirect Source
C**   FSUBY3 = Fy Term (Horizontal Gaussian term) for the
C**            Penetrated Source
C**   HEDx   = Effective Source Heights for Direct Plume,
C**            x corresponding to each of the 2 distributions
C**   HENx   = Effective Source Heights for Indirect Plume,
C**            x corresponding to each of the 2 distributions
C**   HE3    = Effective Source Height for Penetrated Plume
C**   HEDxM  = Effective Source Heights for Direct Plume at Xm,
C**            x corresponding to each of the 2 distributions
C**   HENxM  = Effective Source Heights for Indirect Plume at Xm,
C**            x corresponding to each of the 2 distributions
C**   HE3M   = Effective Source Height for Penetrated Plume at Xm

C**   QSUBN  = Source Term for Indirect Source
C**   QSUB3  = Source Term for Penetrated Source
C**   SKEW   = Skewness of the Vertical Velocity
C**   R      = Lagrangian Correlation Coefficient
C**   ALPHPD = ALPHA Coefficient for the CBL PDF
C**   BETAPD = BETA  Coefficient for the CBL PDF
C**   ASUB1  =
C**   ASUB2  =
C**   BSUB1  =
C**   BSUB2  =
C**   LAMDA1 = Relative Frequencies of Updrafts
C**   LAMDA2 = Relative Frequencies of Downdrafts

C**   QSUM        = Sum of emissions for merged plumes in PVMRM option
C**   SUM_NO2RAT  = Sum of NO2/NOx ratios for merged plumes in PVMRM

      DOUBLE PRECISION :: QSUM, SUM_NO2RAT
      
      DOUBLE PRECISION :: CWMAX, CWMIN, DWMAX, DWMIN
      DOUBLE PRECISION :: HMNH,  HMXH,  HMNT,  HMXT, HMNH3, HMXH3, 
     &                    HMNT3, HMXT3

C***********************************************************************
C     This is The Global Variable Definition Block for EVent Pathway
C***********************************************************************

      CHARACTER EVNAME*10, EVGRP*8
      INTEGER, ALLOCATABLE ::  EVAPER(:), EVDATE(:), EVJDAY(:),
     &                         IDXEV(:)

      ALLOCATABLE ::  EVNAME(:), EVGRP(:)




C***********************************************************************
C     This is The Global Variable Definition Block for OUtput Pathway
C***********************************************************************

      LOGICAL OUTPART, SUMMFILE, L_NoHeader(8), EVALFIL, TOXXFIL

      LOGICAL, ALLOCATABLE :: ANPART(:), ALLPARTS(:), ALLPARTG(:)

      CHARACTER (LEN=ILEN_FLD) :: THRFIL, PSTFIL, PLTFIL, ANNPST,
     &                            ANNPLT, THRFRM, PSTFRM, PLTFRM,
     &                            TOXFIL, SEAHRS, RNKFIL, RNKFRM,
     &                            EVLFIL, SUMFIL, MXDFRM,
     &                            MAXDLY, MAXDLY_BYYR, MAXDCONT_FILE

C --- Variable for specifying format for file outputs (default = 'FIX')
      CHARACTER (LEN = 3) :: FILE_FORMAT
      
      INTEGER, ALLOCATABLE :: NHIAVE(:,:), MAXAVE(:), IMXVAL(:),
     &                        IDYTAB(:), MAXFLE(:,:), IPSTFL(:,:), 
     &                        IPLTFL(:,:,:), IANPST(:), IANPLT(:), 
     &                        INHI(:), ITOXFL(:), ISEAHR(:), 
     &                        IMXDLY(:), IMXDLY_BYYR(:), MAXDCONT(:),
     &                        IRNKFL(:), IRKVAL(:), IANPART(:)
     
      DOUBLE PRECISION, ALLOCATABLE :: THRESH(:,:), TOXTHR(:),
     &                                 MAXD_THRESH(:)
     
      DOUBLE PRECISION, ALLOCATABLE :: AXR_SAV(:), AYR_SAV(:), 
     &                                 AZELEV_SAV(:), AZFLAG_SAV(:), 
     &                                 AZHILL_SAV(:)

      INTEGER, ALLOCATABLE :: IMXUNT(:,:), IPSUNT(:,:), IPSFRM(:,:), 
     &                        IPLUNT(:,:,:), IAPUNT(:), 
     &                        IANFRM(:), IPPUNT(:), ITXUNT(:), 
     &                        IRKUNT(:), IELUNT(:), IUPART(:),
     &                        ISHUNT(:), IMDUNT(:), IMDUNT_BYYR(:),
     &                        IMXDCUNT(:), MXD_RANK(:,:)

      ALLOCATABLE ::  THRFIL(:,:), PSTFIL(:,:), PLTFIL(:,:,:), 
     &                ANNPST(:), ANNPLT(:), TOXFIL(:), SEAHRS(:),
     &                RNKFIL(:), EVLFIL(:), MAXDLY(:),
     &                MAXDLY_BYYR(:), MAXDCONT_FILE(:)

      INTEGER, ALLOCATABLE :: IDCONC(:,:)

      INTEGER :: ITAB, NXTOX, NYTOX, NHOURS, IPAIR

      DOUBLE PRECISION, ALLOCATABLE :: TXCONC(:,:)



C***********************************************************************
C     This is The Global Variable Definition Block for Working Space
C***********************************************************************

      CHARACTER WORKID*12, DUMMY*12

      INTEGER :: IMIT, INUM, IDUM, INDAVE, INDGRP, INDVAL,
     &           ISC, IOERRN, NCPP, NRPP, NGPP, NPPX, NPPY
      REAL :: FNUM
      DOUBLE PRECISION :: DNUM

      ALLOCATABLE ::          WORKID(:)
      INTEGER, ALLOCATABLE :: IWRK2(:,:)

C     Declare Temporary Work Arrays for ZELEV and ZFLAG Receptor Data
      DOUBLE PRECISION, ALLOCATABLE :: ZETMP1(:), ZETMP2(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZFTMP1(:), ZFTMP2(:)
      DOUBLE PRECISION, ALLOCATABLE :: ZHTMP1(:), ZHTMP2(:)


C***********************************************************************
C     Formerly MAIN3.INC
C     ISCST2 Model Data - Array Names, Array Limits, Named Common Blocks
C                         Necessary for Model Results
C     MODIFIED - 4/17/95   Output CONC/DEPOS in same model run
C***********************************************************************


C***********************************************************************
C     This is The Global Variable Definition Block For The Maximum
C     Value, Highest Value, Average Value, Annual Average Value and
C     Model Result Arrays.  Also Included are Calm/Missing Flag Arrays.
C***********************************************************************


      CHARACTER HCLMSG, MCLMSG, HMCLM

      DOUBLE PRECISION, ALLOCATABLE ::  HRVAL(:), AVEVAL(:,:,:,:),
     &                                  AERVAL(:), PRMVAL(:)
      DOUBLE PRECISION, ALLOCATABLE ::  HIVALU(:,:,:,:,:),
     &                                  HMAX(:,:,:,:)
      INTEGER, ALLOCATABLE ::  HMLOC(:,:,:,:),
     &                         HMDATE(:,:,:,:),
     &                         NHIDAT(:,:,:,:,:),
     &                         NHIDATMXD(:,:,:),
     &                         NHIDATMXD_BYYR(:,:,:,:)

      DOUBLE PRECISION, ALLOCATABLE ::  ANNVAL(:,:,:), AMXVAL(:,:,:),
     &                                  SHVALS(:,:,:,:,:), MXDVAL(:,:), 
     &                                  HIMXDLY(:,:,:),
     &                                  HIMXDLY_BYYR(:,:,:,:)
      INTEGER, ALLOCATABLE ::  IMXLOC(:,:,:), IMXDHR(:,:)
      INTEGER              ::  IANHRS, IANCLM, IANMSG,
     &                         NSEAHR(4,24), NSEACM(4,24)
      DOUBLE PRECISION, ALLOCATABLE ::  RMXVAL(:,:,:,:)
      INTEGER, ALLOCATABLE ::  MXDATE(:,:,:,:),
     &                         MXLOCA(:,:,:,:)
      INTEGER, ALLOCATABLE ::  NUMHRS(:), NUMCLM(:), NUMMSG(:)
      ALLOCATABLE ::           HCLMSG(:,:,:,:,:),
     &                         MCLMSG(:,:,:,:),
     &                         HMCLM(:,:,:,:)

      DOUBLE PRECISION, ALLOCATABLE ::  SUMANN(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE ::  SUMHNH(:,:,:), MXPMVAL(:,:,:)
      DOUBLE PRECISION, ALLOCATABLE ::  SUMVAL_MAXD(:,:,:,:)

      INTEGER, ALLOCATABLE ::  MXPMLOC(:,:,:)

      DOUBLE PRECISION, ALLOCATABLE ::  CHI(:,:,:), HECNTR(:,:), 
     &                                  HECNTR3(:,:), PPFACT(:),
     &                                  UEFFS(:,:), UEFF3S(:,:),
     &                                  FOPTS(:,:), 
     &                                  ABVAL(:,:), BCVAL(:,:)

      DOUBLE PRECISION, ALLOCATABLE ::  ARCMAX(:), QMAX(:), DXMAX(:),
     &                                  UMAX(:),
     &                        SVMAX(:), SWMAX(:), SYMAX(:), SY3MX(:),
     &                        U3MAX(:), HEMAX(:), ARCCL(:), SZMAX(:),
     &                        CHIDMW(:), CHINMW(:), CHI3MW(:),
     &                        CHIDML(:), CHINML(:), CHI3ML(:),
     &                        HSBLMX(:)

      LOGICAL, ALLOCATABLE :: CHIMASK(:,:,:)

C***********************************************************************
C     This is The Global Variable Definition Block For The
C     EVENT Model Result Arrays
C***********************************************************************

      DOUBLE PRECISION, ALLOCATABLE ::  EV_AVEVAL(:), HRVALS(:,:), 
     &                                  GRPVAL(:), BACKHR(:)

      DOUBLE PRECISION ::  GRPAVE, BACKAVE
      INTEGER ::  EV_NUMHRS, EV_NUMCLM, EV_NUMMSG, ISTAHR, IENDHR


C***********************************************************************
C    
C     BLOCK DATA area for initializing global data
C
C***********************************************************************
C***********************************************************************
C     Initialize Model Version Number, VERSN (Year, Julian Day), as a
C     Character Variable
C***********************************************************************

C---- VERSN is now a 6-character variable to accomodate leading qualifier
C     character, such as 'B' for Beta version or 'D' for Draft version.
      DATA VERSN /' 12060'/


C***********************************************************************
C     Input/Output File Units and Input/Output File Names
C***********************************************************************

      DATA INUNIT/ 7/, IOUNIT/ 8/, PVMDBG/ 9/, IERUNT/10/, IERWRT/11/, 
     &     IDPUNT/12/, IBGUNT/13/, IDPUN2/14/, IRSUNT/15/, IHREMI/16/, 
     &     IEVUNT/17/, ITEVUT/18/, MFUNIT/19/, INCUNT/20/, MPUNIT/21/, 
     &     ISUNIT/22/, IPUNIT/23/, DBGUNT/24/, DBMUNT/25/, ICVUNT/26/, 
     &     ISUMUNT/27/, IO3UNT/28/, GDEPDBG/29/, PDEPDBG/30/
C*#
      DATA INPFIL/' '/, OUTFIL/' '/


C***********************************************************************
C     Initialize Keyword Array
C***********************************************************************

      INTEGER, PRIVATE :: I

      DATA (KEYWD(I),I=1,IKN) /
     &   'STARTING','FINISHED','TITLEONE','TITLETWO','MODELOPT',
     &   'AVERTIME','POLLUTID','HALFLIFE','DCAYCOEF','DEBUGOPT',
     &   'ELEVUNIT','FLAGPOLE','RUNORNOT','EVENTFIL','SAVEFILE',
     &   'INITFILE','MULTYEAR','ERRORFIL','GASDEPDF','GDSEASON',
     &   'GDLANUSE','GASDEPVD','URBANOPT','NO2EQUIL','NO2STACK',
     &   'OZONEVAL','OZONEFIL','O3VALUES','OZONUNIT',
     &   'LOCATION','SRCPARAM','BUILDHGT','BUILDWID','BUILDLEN',
     &   'XBADJ   ','YBADJ   ','EMISFACT','EMISUNIT','PARTDIAM',
     &   'MASSFRAX','PARTDENS','METHOD_2','CONCUNIT','DEPOUNIT',
     &   'GASDEPOS','HOUREMIS','NO2RATIO','AREAVERT','URBANSRC',
     &   'SRCGROUP','OLMGROUP','PSDGROUP','BACKGRND','BACKUNIT',
     &   'INCLUDED','EVENTPER','EVENTLOC','GRIDCART','GRIDPOLR',
     &   'DISCCART','DISCPOLR','EVALCART','SURFFILE','PROFFILE',
     &   'PROFBASE','SURFDATA','UAIRDATA','SITEDATA','STARTEND',
     &   'DAYRANGE','SCIMBYHR','WDROTATE','WINDCATS','NUMYEARS',
     &   'RECTABLE','MAXTABLE','DAYTABLE','SUMMFILE','MAXIFILE',
     &   'POSTFILE','PLOTFILE','TOXXFILE','SEASONHR','EVENTOUT',
     &   'RANKFILE','EVALFILE','FILEFORM','MAXDAILY','MXDYBYYR',
     &   'MAXDCONT','NOHEADER'/


C***********************************************************************
C     Initialize Miscellaneous Variables
C***********************************************************************

      DATA IPROC /366*1/, EXPLIM /-50.0D0/
      DATA UCAT /1.54D0, 3.09D0, 5.14D0, 8.23D0, 10.8D0/
      DATA MODOPS /20*'         '/

C***********************************************************************
C     Initialize distance factors used in determining when to switch
C     to point source approximation for area sources under the FASTAREA
C     option (formerly the TOXICS option).
C***********************************************************************

C     STAB. CLASS         A      B      C       D       E       F
C                        ***    ***    ***     ***     ***     ***
      DATA VIRTPNT_URB /3.5D0, 3.5D0, 5.5D0, 10.5D0, 15.5D0, 15.5D0/,
     &     VIRTPNT_RUR /3.5D0, 5.5D0, 7.5D0, 12.5D0, 15.5D0, 25.5D0/


C***********************************************************************
C     Initialize Setup Status Arrays
C***********************************************************************

      DATA ICSTAT/50*0/, ISSTAT/50*0/, IRSTAT/50*0/, IMSTAT/50*0/,
     &     IOSTAT/50*0/, IESTAT/50*0/

C***********************************************************************
C     Cross-reference table of Keywords by I?STAT array index:
C     
C     Array    Index  Keyword            Array    Index  Keyword
C     -----    -----  -------            -----    -----  -------    
C     ICSTAT:	 1 = starting            ISSTAT:	 1 = starting
C      		     2 = titleone                        2 = location
C       		 3 = titletwo       		         3 = srcparam
C       		 4 = modelopt       		         4 = buildhgt
C       		 5 = avertime       		         5 = buildwid
C       		 6 = pollutid       		         7 = emisfact
C       		 7 = halflife       		         8 = emisunit
C       		 8 = dcaycoef       		         9 = partdiam
C       		11 = flagpole       		        10 = massfrax
C       		12 = runornot       		        11 = partdens
C       		13 = eventfil       		        15 = elevunit
C       		14 = savefile       		        16 = houremis
C       		15 = initfile       		        17 = concunit
C       		16 = multyear       		        18 = depounit
C       		17 = errorfil       		        19 = areavert
C       		18 = gdseason       		        20 = included
C       		19 = gasdepdf       		        21 = buildlen
C       		20 = gdlanuse       		        22 = xbadj   
C       		21 = gasdepvd       		        23 = ybadj   
C       		22 = debugopt       		        24 = srcgroup
C       		23 = urbanopt       		        26 = gasdepos
C       		24 = ozoneval       		        27 = method_2
C       		26 = ozonefil       		        28 = urbansrc
C       		27 = no2equil       		        29 = no2ratio
C       		28 = no2stack       		        30 = olmgroup
C       		50 = finished       		        34 = psdgroup
C   						     	                    50 = finished
C
C     IRSTAT:    1 = starting            IESTAT:     1 = starting
C                2 = gridcart            (EVENT)     2 = eventper
C                3 = gridpolr                        3 = eventloc
C                4 = disccart                       10 = included
C                5 = discpolr                       50 = finished
C                8 = evalcart    
C                9 = elevunit    
C               11 = included    
C               50 = finished    
C    
C     IMSTAT:    1 = starting
C                2 = surffile
C                3 = proffile
C                4 = surfdata
C                5 = uairdata
C                6 = startend
C                7 = dayrange
C                8 = wdrotate
C                9 = sitedata
C               10 = profbase
C               11 = windcats
C               12 = scimbyhr
C               13 = numyears
C               50 = finished
C    
C     IOSTAT:    1 = starting            IOSTAT:     1 = starting
C (non-EVENT)    2 = rectable            (EVENT)     2 = eventout
C                3 = maxtable                       25 = finished
C                4 = daytable
C                5 = maxifile
C                6 = postfile
C                7 = plotfile
C                8 = toxxfile
C                9 = seasonhr
C               10 = rankfile
C               11 = evalfile
C               12 = summfile
C               13 = fileform
C               50 = finished
C                                            
C***********************************************************************


C***********************************************************************
C     Initialize Gridded Profile Height Array
C***********************************************************************

      DATA GRIDHT / 
     &   0.0D0, 0.5D0, 1.0D0, 2.0D0, 4.0D0,  8.0D0, 14.0D0, 20.0D0,
     &  30.0D0,  40.0D0,  50.0D0,  60.0D0,  70.0D0,  80.0D0,  90.0D0,
     &  100.0D0, 120.0D0, 140.0D0, 160.0D0, 180.0D0, 200.0D0, 250.0D0,
     &  300.0D0, 350.0D0, 400.0D0, 450.0D0, 500.0D0, 550.0D0, 600.0D0,
     &  650.0D0, 700.0D0, 750.0D0, 800.0D0, 850.0D0, 900.0D0, 950.0D0,
     & 1000.0D0, 1050.0D0, 1100.0D0, 1150.0D0, 1200.0D0, 1250.0D0,
     & 1300.0D0, 1350.0D0, 1400.0D0, 1450.0D0, 1500.0D0, 1550.0D0,
     & 1600.0D0, 1650.0D0, 1700.0D0, 1750.0D0, 1800.0D0, 1850.0D0,
     & 1900.0D0, 1950.0D0, 2000.0D0, 2100.0D0, 2200.0D0, 2300.0D0,
     & 2400.0D0, 2500.0D0, 2600.0D0, 2700.0D0, 2800.0D0, 2900.0D0,
     & 3000.0D0, 3100.0D0, 3200.0D0, 3300.0D0, 3400.0D0, 3500.0D0,
     & 3600.0D0, 3700.0D0, 3800.0D0, 3900.0D0, 4000.0D0, 4100.0D0,
     & 4200.0D0, 4300.0D0, 4400.0D0, 4500.0D0, 4600.0D0, 4700.0D0,
     & 4800.0D0, 4900.0D0, 5000.0D0/


C***********************************************************************
C     Initialize Error Code and Message Arrays
C***********************************************************************

      DATA ERRCOD(  1)/'100'/,
     & ERRMSG(  1)/'Invalid Pathway Specified. The Troubled Pathway is'/
      DATA ERRCOD(  2)/'105'/,
     & ERRMSG(  2)/'Invalid Keyword Specified. The Troubled Keyword is'/
      DATA ERRCOD(  3)/'109'/,
     & ERRMSG(  3)/'Too many fields specified on runstream image; MAX='/
      DATA ERRCOD(  4)/'110'/,
     & ERRMSG(  4)/'Keyword is Not Valid for This Pathway.  Keyword is'/
      DATA ERRCOD(  5)/'115'/,
     & ERRMSG(  5)/'STARTING or FINISHED Out of Sequence:  Pathway =  '/
      DATA ERRCOD(  6)/'120'/,
     & ERRMSG(  6)/'Pathway is Out of Sequence:  Pathway =            '/
      DATA ERRCOD(  7)/'125'/,
     & ERRMSG(  7)/'Missing FINISHED-Runstream File Incomplete: ISTAT='/
      DATA ERRCOD(  8)/'130'/,
     & ERRMSG(  8)/'Missing Mandatory Keyword.  The Missing Keyword is'/
      DATA ERRCOD(  9)/'135'/,
     & ERRMSG(  9)/'Nonrepeatable Keyword or Recursed INCLUDED: Keywrd'/
      DATA ERRCOD( 10)/'140'/,
     & ERRMSG( 10)/'Invalid Order of Keyword.  The Troubled Keyword is'/
      DATA ERRCOD( 11)/'141'/,
     & ERRMSG( 11)/'Conflicting Options:  PVMRM and OLM both specified'/
      DATA ERRCOD( 12)/'142'/,
     & ERRMSG( 12)/'Following Keyword Invalid Without PVMRM or OLM:   '/
      DATA ERRCOD( 13)/'143'/,
     & ERRMSG( 13)/'Following Keyword Invalid Without PVMRM Option:   '/
      DATA ERRCOD( 14)/'144'/,
     & ERRMSG( 14)/'Following Keyword Invalid Without OLM Option:     '/
      DATA ERRCOD( 15)/'145'/,
     & ERRMSG( 15)/'Conflicting Options: MULTYEAR and Re-Start Option '/
      DATA ERRCOD( 16)/'146'/,
     & ERRMSG( 16)/'PSDGROUP Keyword Specified without PSDCREDIT Opt. '/
      DATA ERRCOD( 17)/'147'/,
     & ERRMSG( 17)/'Following Option is Invalid with PSDCREDIT Option:'/
      DATA ERRCOD( 18)/'148'/,
     & ERRMSG( 18)/'O3VALUES keyword will be used instead of OZONEVAL '/
      DATA ERRCOD( 19)/'149'/,
     & ERRMSG( 19)/'Conflicting options specified on MODELOPT keyword:'/
      DATA ERRCOD( 20)/'150'/,
     & ERRMSG( 20)/'Conflicting Options: MULTYEAR for Wrong Pollutant '/
      DATA ERRCOD( 21)/'151'/,
     & ERRMSG( 21)/'Non-DFAULT NoUrbTran option selected on MODELOPT  '/
      DATA ERRCOD( 22)/'152'/,
     & ERRMSG( 22)/'ELEVUNIT card must be first for this Pathway:     '/
      DATA ERRCOD( 23)/'153'/,
     & ERRMSG( 23)/'Conflicting Opts: MAXDCONT with Re-Start or MULTYR'/
      DATA ERRCOD( 24)/'154'/,
     & ERRMSG( 24)/'Conflicting options:  SCIM cannot be used with    '/
      DATA ERRCOD( 25)/'155'/,
     & ERRMSG( 25)/'Conflicting Decay Keyword. Inputs Ignored for     '/
      DATA ERRCOD( 26)/'156'/,
     & ERRMSG( 26)/'Option ignored - not valid with SCIM.  Option =   '/
      DATA ERRCOD( 27)/'157'/,
     & ERRMSG( 27)/'Wet SCIM Not Supported - Wet SCIM Inputs Ignored  '/
      DATA ERRCOD( 28)/'158'/,
     & ERRMSG( 28)/'EMISUNIT Keyword Used With More Than 1 Output Type'/
      DATA ERRCOD( 29)/'159'/,
     & ERRMSG( 29)/'EMISUNIT Keyword Used With the Following Keyword: '/
      DATA ERRCOD( 30)/'160'/,
     & ERRMSG( 30)/'Duplicate ORIG Secondary Keyword for GRIDPOLR:    '/
      DATA ERRCOD( 31)/'161'/,
     & ERRMSG( 31)/'MAXDCONT option already defined for source group: '/
      DATA ERRCOD( 32)/'162'/,
     & ERRMSG( 32)/'Option only applies to 1-hr NO2 or 1-hr SO2 NAAQS:'/
      DATA ERRCOD( 33)/'163'/,
     & ERRMSG( 33)/'Option only applies to 24h PM25, 1h NO2 or 1h SO2:'/
      DATA ERRCOD( 34)/'164'/,
     & ERRMSG( 34)/'NOHEADER selected for non-specified output option:'/
      DATA ERRCOD( 35)/'165'/,
     & ERRMSG( 35)/'Inconsistent temporally-varying BACKGRND options: '/
      DATA ERRCOD( 36)/'166'/,
     & ERRMSG( 36)/'HOURLY BACKGRND option specified more than once on'/
      DATA ERRCOD( 37)/'170'/,
     & ERRMSG( 37)/'Invalid Secondary Keyword for Receptor Grid:      '/
      DATA ERRCOD( 38)/'175'/,
     & ERRMSG( 38)/'Missing Secondary Keyword END for Receptor Grid:  '/
      DATA ERRCOD( 39)/'180'/,
     & ERRMSG( 39)/'Conflicting Secondary Keyword for Receptor Grid:  '/
      DATA ERRCOD( 40)/'185'/,
     & ERRMSG( 40)/'Missing Receptor Keywords. No Receptors Specified.'/
      DATA ERRCOD( 41)/'189'/,
     & ERRMSG( 41)/'No Keywords for OU Path and No PERIOD/ANNUAL Aves.'/
      DATA ERRCOD( 42)/'190'/,
     & ERRMSG( 42)/'Incompatible Option Used With SAVEFILE or INITFILE'/
      DATA ERRCOD( 43)/'191'/,
     & ERRMSG( 43)/'PM25, 1h NO2 or SO2 w/o MAXIFILE incompatible with'/
      DATA ERRCOD( 44)/'192'/,
     & ERRMSG( 44)/'FASTALL option also implies use of FASTAREA option'/
      DATA ERRCOD( 45)/'193'/,
     & ERRMSG( 45)/'Units keyword specified without appropriate option'/
      DATA ERRCOD( 46)/'194'/,
     & ERRMSG( 46)/'Option specified on DEBUGOPT keywrd not applicable'/
      DATA ERRCOD( 47)/'195'/,
     & ERRMSG( 47)/'Incompatible Keyword used with GASDEPVD option    '/
      DATA ERRCOD( 48)/'196'/,
     & ERRMSG( 48)/'Gas deposition algorithms are non-DFAULT options  '/
      DATA ERRCOD( 49)/'197'/,
     & ERRMSG( 49)/'METHOD_2 for particulates is a non-DFAULT option  '/
      DATA ERRCOD( 50)/'198'/,
     & ERRMSG( 50)/'TOXICS Option obsolete; see Users Guide Addendum  '/
      DATA ERRCOD( 51)/'199'/,
     & ERRMSG( 51)/'Non-DFAULT BETA Option Required for               '/
     
      DATA ERRCOD( 52)/'200'/,
     & ERRMSG( 52)/'Missing Parameter(s). No Options Specified For    '/
      DATA ERRCOD( 53)/'201'/,
     & ERRMSG( 53)/'Not Enough Parameters Specified For the Keyword of'/
      DATA ERRCOD( 54)/'202'/,
     & ERRMSG( 54)/'Too Many Parameters Specified For the Keyword of  '/
      DATA ERRCOD( 55)/'203'/,
     & ERRMSG( 55)/'Invalid Parameter Specified.  Troubled Parameter: '/
      DATA ERRCOD( 56)/'204'/,
     & ERRMSG( 56)/'Regulatory DFAULT Conflicts with Non-DFAULT Option'/
      DATA ERRCOD( 57)/'205'/,
     & ERRMSG( 57)/'No Option Parameter Setting.  Forced by Default to'/
      DATA ERRCOD( 58)/'206'/,
     & ERRMSG( 58)/'Regulatory DFAULT Overrides Non-DFAULT Option For '/
      DATA ERRCOD( 59)/'207'/,
     & ERRMSG( 59)/'No Parameters Specified. Default Values Will Used.'/
      DATA ERRCOD( 60)/'208'/,
     & ERRMSG( 60)/'Illegal Numerical Field Encountered in            '/
      DATA ERRCOD( 61)/'209'/,
     & ERRMSG( 61)/'Negative Value Appears For Non-negative Variable. '/
      DATA ERRCOD( 62)/'210'/,
     & ERRMSG( 62)/'Number of Short Term Averages Exceeds Max:  NAVE ='/
      DATA ERRCOD( 63)/'211'/,
     & ERRMSG( 63)/'Duplicate Averaging Period Specified for Keyword  '/
      DATA ERRCOD( 64)/'212'/,
     & ERRMSG( 64)/'END Encountered Without (X,Y) Points Properly Set '/
      DATA ERRCOD( 65)/'213'/,
     & ERRMSG( 65)/'ELEV Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD( 66)/'214'/,
     & ERRMSG( 66)/'ELEV Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD( 67)/'215'/,
     & ERRMSG( 67)/'FLAG Input Inconsistent With Option: Input Ignored'/
      DATA ERRCOD( 68)/'216'/,
     & ERRMSG( 68)/'FLAG Input Inconsistent With Option: Defaults Used'/
      DATA ERRCOD( 69)/'217'/,
     & ERRMSG( 69)/'More Than One Delimiter In A Field for Keyword    '/
      DATA ERRCOD( 70)/'218'/,
     & ERRMSG( 70)/'Number of (X,Y) Points Does Not Match Number of   '/
      DATA ERRCOD( 71)/'219'/,
     & ERRMSG( 71)/'Number Of Receptors Specified Exceeds Max:  NREC ='/
      DATA ERRCOD( 72)/'220'/,
     & ERRMSG( 72)/'Missing Origin (Use Default = 0,0) In GRIDPOLR    '/
      DATA ERRCOD( 73)/'221'/,
     & ERRMSG( 73)/'Missing Distance Setting In Polar Network         '/
      DATA ERRCOD( 74)/'222'/,
     & ERRMSG( 74)/'Missing Degree Or Dist Setting In Polar Network   '/
      DATA ERRCOD( 75)/'223'/,
     & ERRMSG( 75)/'Missing Distance or Degree Field in               '/
      DATA ERRCOD( 76)/'224'/,
     & ERRMSG( 76)/'Number of Receptor Networks Exceeds Max:  NNET =  '/
      DATA ERRCOD( 77)/'225'/,
     & ERRMSG( 77)/'Number of X-Coords Specified Exceeds Max:  IXM =  '/
      DATA ERRCOD( 78)/'226'/,
     & ERRMSG( 78)/'Number of Y-Coords Specified Exceeds Max:  IYM =  '/
      DATA ERRCOD( 79)/'227'/,
     & ERRMSG( 79)/'No Receptors Were Defined on the RE Pathway.      '/
      DATA ERRCOD( 80)/'228'/,
     & ERRMSG( 80)/'Default(s) Used for Missing Parameters on Keyword '/
      DATA ERRCOD( 81)/'229'/,
     & ERRMSG( 81)/'Too Many Parameters - Inputs Ignored on Keyword   '/
      DATA ERRCOD( 82)/'230'/,
     & ERRMSG( 82)/'Source ID field is too long (>12); first 12 chars:'/
      DATA ERRCOD( 83)/'231'/,
     & ERRMSG( 83)/'Too Many Numerical Values Specified for           '/
      DATA ERRCOD( 84)/'232'/,
     & ERRMSG( 84)/'Number Of Specified Sources Exceeds Maximum: NSRC='/
      DATA ERRCOD( 85)/'233'/,
     & ERRMSG( 85)/'Building Dimensions Specified for Non-POINT Source'/
      DATA ERRCOD( 86)/'234'/,
     & ERRMSG( 86)/'Too Many Sectors Input for                        '/
      DATA ERRCOD( 87)/'235'/,
     & ERRMSG( 87)/'Number of Source Groups Exceeds Maximum:  NGRP =  '/
      DATA ERRCOD( 88)/'236'/,
     & ERRMSG( 88)/'Not Enough BUILDHGTs Specified for SourceID       '/
      DATA ERRCOD( 89)/'237'/,
     & ERRMSG( 89)/'Not Enough BUILDWIDs Specified for SourceID       '/
      DATA ERRCOD( 90)/'238'/,
     & ERRMSG( 90)/'Not Enough BACKGRND Concentration Values Specified'/
      DATA ERRCOD( 91)/'239'/,
     & ERRMSG( 91)/'Not Enough QFACTs Specified for SourceID          '/
      DATA ERRCOD( 92)/'240'/,
     & ERRMSG( 92)/'Inconsistent Number of Particle Categories for    '/
      DATA ERRCOD( 93)/'241'/,
     & ERRMSG( 93)/'Not Enough BUILDLENs Specified for SourceID       '/
      DATA ERRCOD( 94)/'242'/,
     & ERRMSG( 94)/'No Particle Cat. or Gas Depos. Specified for SRCID'/
      DATA ERRCOD( 95)/'243'/,
     & ERRMSG( 95)/'Wet depos (DEPOS, WDEP, WETDPLT) incompatible with'/
      DATA ERRCOD( 96)/'244'/,
     & ERRMSG( 96)/'Source parameters are missing or incomplete for   '/
      DATA ERRCOD( 97)/'245'/,
     & ERRMSG( 97)/'No. of Particle Categories Exceeds Max:  NPDMAX = '/
      DATA ERRCOD( 98)/'246'/,
     & ERRMSG( 98)/'Not Enough XBADJs Specified for SourceID          '/
      DATA ERRCOD( 99)/'247'/,
     & ERRMSG( 99)/'Not Enough YBADJs Specified for SourceID          '/
      DATA ERRCOD(100)/'248'/,
     & ERRMSG(100)/'No Sources Were Defined on the SO Pathway.        '/
      DATA ERRCOD(101)/'249'/,
     & ERRMSG(101)/'Source elevation is missing (-9999.0); SRCID =    '/
      DATA ERRCOD(102)/'250'/,
     & ERRMSG(102)/'Duplicate XPNT/DIST or YPNT/DIR Specified for GRID'/
      DATA ERRCOD(103)/'252'/,
     & ERRMSG(103)/'Duplicate Receptor Network ID Specified.  NETID = '/
      DATA ERRCOD(104)/'254'/,
     & ERRMSG(104)/'Number of Receptor ARCs Exceeds Max:       NARC = '/
      DATA ERRCOD(105)/'256'/,
     & ERRMSG(105)/'EVALFILE Option Used Without EVALCART Receptors   '/
      DATA ERRCOD(106)/'259'/,
     & ERRMSG(106)/'Receptor elevation is missing (-9999.0); IREC =   '/
      DATA ERRCOD(107)/'260'/,
     & ERRMSG(107)/'Number of EMISFACT/O3VALUES/BACKGRND values > max:'/
      DATA ERRCOD(108)/'261'/,
     & ERRMSG(108)/'Not Enough O3VALUES Ozone Concentrations Specified'/
      DATA ERRCOD(109)/'262'/,
     & ERRMSG(109)/'First Vertex Does Not Match LOCATION for AREAPOLY '/
      DATA ERRCOD(110)/'264'/,
     & ERRMSG(110)/'Too Many Vertices Specified for AREAPOLY Source   '/
      DATA ERRCOD(111)/'265'/,
     & ERRMSG(111)/'Not Enough Vertices Specified for AREAPOLY Source '/
      DATA ERRCOD(112)/'266'/,
     & ERRMSG(112)/'Invalid shape defined (area=0) for AREAPOLY source'/
      DATA ERRCOD(113)/'270'/,
     & ERRMSG(113)/'Number of High Values Specified Exceeds Max: NVAL='/
      DATA ERRCOD(114)/'272'/,
     & ERRMSG(114)/'Upper bound rank > Lower bound rank for MAXDCONT: '/
      DATA ERRCOD(115)/'275'/,
     & ERRMSG(115)/'Number of Max Values Specified Exceeds Max:  NMAX='/
      DATA ERRCOD(116)/'280'/,
     & ERRMSG(116)/'Number of Output Types Specified Exceeds Max:NTYP='/
      DATA ERRCOD(117)/'281'/,
     & ERRMSG(117)/'Number of OLMGROUPs Specified Exceeds Max: NOLM = '/
      DATA ERRCOD(118)/'282'/,
     & ERRMSG(118)/'Following SRCID Included in Multiple OLMGROUPs:   '/
      DATA ERRCOD(119)/'283'/,
     & ERRMSG(119)/'Either OZONEVAL or OZONEFIL Card Needed for Option'/
      DATA ERRCOD(120)/'284'/,
     & ERRMSG(120)/'Invalid POLLUTID Specified for PVMRM/OLM; Must Use'/
      DATA ERRCOD(121)/'285'/,
     & ERRMSG(121)/'Number of Urban Areas Exceeds Maximum.  NURB =    '/
      DATA ERRCOD(122)/'286'/,
     & ERRMSG(122)/'Following SRCID Included in Multiple PSDGROUPs:   '/
      DATA ERRCOD(123)/'287'/,
     & ERRMSG(123)/'PSDGROUP ID Must be INCRCONS, RETRBASE or NONRBASE'/
      DATA ERRCOD(124)/'288'/,
     & ERRMSG(124)/'Use of "*" for repeated values not meaningful for '/
      DATA ERRCOD(125)/'289'/,
     & ERRMSG(125)/'Source defined as both particulate and gaseous    '/
      DATA ERRCOD(126)/'290'/,
     & ERRMSG(126)/'Number of Events Specified Exceeds Max:   NEVE =  '/
      DATA ERRCOD(127)/'291'/,
     & ERRMSG(127)/'Filename specified is too long. Maximum length =  '/
      DATA ERRCOD(128)/'292'/,
     & ERRMSG(128)/'Potential problem with Fortran format specifier:  '/
      DATA ERRCOD(129)/'293'/,
     & ERRMSG(129)/'User-specified met data format not used;  use FREE'/
      DATA ERRCOD(130)/'294'/,
     & ERRMSG(130)/'PERIOD and ANNUAL averages are both selected for  '/
      DATA ERRCOD(131)/'295'/,
     & ERRMSG(131)/'Invalid Averaging Period Specified for SCREEN Mode'/
      DATA ERRCOD(132)/'296'/,
     & ERRMSG(132)/'Averaging Period .NE. 1-Hr for TOXXFILE Option    '/
      DATA ERRCOD(133)/'297'/,
     & ERRMSG(133)/'Aver. Period must be .LE. 24 for EVENT Processing '/
      DATA ERRCOD(134)/'298'/,
     & ERRMSG(134)/'Error Allocating Storage for Setup Arrays!        '/
      DATA ERRCOD(135)/'299'/,
     & ERRMSG(135)/'Error Allocating Storage for Result Arrays!       '/
     
      DATA ERRCOD(136)/'300'/,
     & ERRMSG(136)/'Specified SRCID Has Not Been Defined Yet: KEYWORD='/
      DATA ERRCOD(137)/'301'/,
     & ERRMSG(137)/'Urban Area ID Has Not Been Defined.  URBID =      '/
      DATA ERRCOD(138)/'302'/,
     & ERRMSG(138)/'Following SRCID Included in Multiple Urban Areas: '/
      DATA ERRCOD(139)/'303'/,
     & ERRMSG(139)/'Urban ID has already been defined.  URBID =       '/
      DATA ERRCOD(140)/'305'/,
     & ERRMSG(140)/'Stack height > or = EPA formula height for SRCID: '/
      DATA ERRCOD(141)/'310'/,
     & ERRMSG(141)/'Attempt to Define Duplicate LOCATION Card for SRC:'/
      DATA ERRCOD(142)/'313'/,
     & ERRMSG(142)/'Attempt to Define Duplicate EVENTPER card for     '/
      DATA ERRCOD(143)/'315'/,
     & ERRMSG(143)/'Attempt to Define Duplicate SRCPARAM Card for SRC:'/
      DATA ERRCOD(144)/'317'/,
     & ERRMSG(144)/'Specified SRCID not included in any PSD/SRCGROUP: '/
      DATA ERRCOD(145)/'318'/,
     & ERRMSG(145)/'No Sources Defined for Urban Area.  URBID =       '/
      DATA ERRCOD(146)/'319'/,
     & ERRMSG(146)/'No Sources Included in Specified Source Group:    '/
      DATA ERRCOD(147)/'320'/,
     & ERRMSG(147)/'Input Parameter May Be Out-of-Range for Parameter '/
      DATA ERRCOD(148)/'322'/,
     & ERRMSG(148)/'Release Height Exceeds Effective Depth for OPENPIT'/
      DATA ERRCOD(149)/'324'/,
     & ERRMSG(149)/'Release Height Exceeds 3000 Meters for SRCID:     '/
      DATA ERRCOD(150)/'325'/,
     & ERRMSG(150)/'Negative Exit Velocity (Set=1.0E-5) for SRCID:    '/
      DATA ERRCOD(151)/'330'/,
     & ERRMSG(151)/'Mass Fraction Parameters Do Not Sum to 1. for Src '/
      DATA ERRCOD(152)/'332'/,
     & ERRMSG(152)/'Mass Fraction Parameter Out-of-Range for Source   '/
      DATA ERRCOD(153)/'334'/,
     & ERRMSG(153)/'Particle Density Out-of-Range for Source          '/
      DATA ERRCOD(154)/'335'/,
     & ERRMSG(154)/'Particle Diameter Out-of-Range for Source         '/
      DATA ERRCOD(155)/'336'/,
     & ERRMSG(155)/'NO2RATIO Invalid or Not Specified for PVMRM Source'/
      DATA ERRCOD(156)/'338'/,
     & ERRMSG(156)/'Neg Emis Rate Cannot be Used with OLM/PVMRM. Src: '/
      DATA ERRCOD(157)/'340'/,
     & ERRMSG(157)/'Possible Error in PROFBASE Input:  Value is < 0   '/
      DATA ERRCOD(158)/'342'/,
     & ERRMSG(158)/'Src ID Mismatch in Hourly Emissions File for ID = '/
      DATA ERRCOD(159)/'344'/,
     & ERRMSG(159)/'Missing HOUREMIS fields; EmisRate set = 0. KURDAT='/
      DATA ERRCOD(160)/'345'/,
     & ERRMSG(160)/'Problem processing the HOUREMIS file.   KURDAT =  '/
      DATA ERRCOD(161)/'346'/,
     & ERRMSG(161)/'Too many fields for HOUREMIS file.     KURDAT =   '/
      DATA ERRCOD(162)/'350'/,
     & ERRMSG(162)/'Julian Day Out Of Range at                        '/
      DATA ERRCOD(163)/'352'/,
     & ERRMSG(163)/'The "H6H" field is no longer required for MULTYEAR'/
      DATA ERRCOD(164)/'353'/,
     & ERRMSG(164)/'Urban Roughness Length (m) May Be Out-of-Range:   '/
      DATA ERRCOD(165)/'360'/,
     & ERRMSG(165)/'2-Digit Year Specified: Valid for Range 1950-2049 '/
      DATA ERRCOD(166)/'361'/,
     & ERRMSG(166)/'Multiyear PERIOD/ANNUAL values for NO2/SO2 require'/
      DATA ERRCOD(167)/'362'/,
     & ERRMSG(167)/'Multiyear 1h NO2/SO2 processing not applicable for'/
      DATA ERRCOD(168)/'363'/,
     & ERRMSG(168)/'Multiyr 24h/Ann PM25 processing not applicable for'/
      DATA ERRCOD(169)/'365'/,
     & ERRMSG(169)/'Year Input is Greater Than 2147                   '/
      DATA ERRCOD(170)/'370'/,
     & ERRMSG(170)/'Invalid Date: 2/29 In a Non-leap Year.            '/
      DATA ERRCOD(171)/'380'/,
     & ERRMSG(171)/'This Input Variable is Out-of-Range:              '/
      DATA ERRCOD(172)/'381'/,
     & ERRMSG(172)/'Latitude in Surface File Is Not Valid:            '/
      DATA ERRCOD(173)/'382'/,
     & ERRMSG(173)/'Error Decoding Latitude:                          '/
      DATA ERRCOD(174)/'384'/,
     & ERRMSG(174)/'Not enough fields specified for HOUREMIS; KURDAT ='/
      DATA ERRCOD(175)/'386'/,
     & ERRMSG(175)/'PARTDIAM and METHOD_2 specified for same SRCID:   '/
      DATA ERRCOD(176)/'387'/,
     & ERRMSG(176)/'METHOD_2 option already specified for this SRCID: '/
      DATA ERRCOD(177)/'391'/,
     & ERRMSG(177)/'Aspect ratio (L/W) of area source greater than 100'/
      DATA ERRCOD(178)/'392'/,
     & ERRMSG(178)/'Aspect ratio (L/W) of open pit is greater than 10 '/
      DATA ERRCOD(179)/'394'/,
     & ERRMSG(179)/'Met data may be from outdated version of AERMET:  '/
      DATA ERRCOD(180)/'395'/,
     & ERRMSG(180)/'Met. Data Error; Incompatible Version of AERMET:  '/
      DATA ERRCOD(181)/'396'/,
     & ERRMSG(181)/'Met data from outdated version of AERMET, version:'/
      DATA ERRCOD(182)/'397'/,
     & ERRMSG(182)/'SCREEN option used without use of SCREEN Met Data '/
      DATA ERRCOD(183)/'398'/,
     & ERRMSG(183)/'SCREEN met used without specifying SCREEN option  '/
      DATA ERRCOD(184)/'399'/,
     & ERRMSG(184)/'EXP format specified with no applicable file types'/
     
      DATA ERRCOD(185)/'400'/,
     & ERRMSG(185)/'Output values exceed format limit; use OU FILEFORM'/
      DATA ERRCOD(186)/'405'/,
     & ERRMSG(186)/'Value of PHEE Exceeds 1.0 on KURDAT =             '/
      DATA ERRCOD(187)/'406'/,
     & ERRMSG(187)/'Number of Vertices Exceeds Max (NVMAX) for SRCID: '/
      DATA ERRCOD(188)/'410'/,
     & ERRMSG(188)/'Wind Direction Out-of-Range.  KURDAT =            '/
      DATA ERRCOD(189)/'413'/,
     & ERRMSG(189)/'Number of Threshold Events > 999999 for Ave Period'/
      DATA ERRCOD(190)/'415'/,
     & ERRMSG(190)/'MAXDCONT THRESH not reached within range of ranks '/
      DATA ERRCOD(191)/'420'/,
     & ERRMSG(191)/'Wind Speed Out-of-Range.   KURDAT =               '/
      DATA ERRCOD(192)/'430'/,
     & ERRMSG(192)/'Ambient Temperature Data Out-of-Range.  KURDAT =  '/
      DATA ERRCOD(193)/'432'/,
     & ERRMSG(193)/'Friction Velocity Out-of-Range.   KURDAT =        '/
      DATA ERRCOD(194)/'435'/,
     & ERRMSG(194)/'Surface Roughness Length Out-of-Range.  KURDAT =  '/
      DATA ERRCOD(195)/'438'/,
     & ERRMSG(195)/'Convective Velocity Data Out-of-Range.  KURDAT =  '/
      DATA ERRCOD(196)/'439'/,
     & ERRMSG(196)/'Monin-Obukhov Length Out-of-Range.  KURDAT =      '/
      DATA ERRCOD(197)/'440'/,
     & ERRMSG(197)/'Calm Hour Identified in Meteorology Data File at  '/
      DATA ERRCOD(198)/'441'/,
     & ERRMSG(198)/'Vert Pot Temp Grad abv ZI set to min .005, KURDAT='/
      DATA ERRCOD(199)/'442'/,
     & ERRMSG(199)/'Vert Pot Temp Grad abv ZI exceeds 0.1 K/m, KURDAT='/
      DATA ERRCOD(200)/'450'/,
     & ERRMSG(200)/'Record Out of Sequence in Meteorological File at: '/
      DATA ERRCOD(201)/'452'/,
     & ERRMSG(201)/'Missing value in hourly BACKGRND file at KURDAT = '/
      DATA ERRCOD(202)/'453'/,
     & ERRMSG(202)/'Substitution for missing hrly BACKGRND data. Date='/
      DATA ERRCOD(203)/'454'/,
     & ERRMSG(203)/'Date/time Mismatch: Hourly BACKGRND file; KURDAT ='/
      DATA ERRCOD(204)/'455'/,
     & ERRMSG(204)/'Date/time Mismatch: Hourly Emission File. KURDAT ='/
      DATA ERRCOD(205)/'456'/,
     & ERRMSG(205)/'Date/time Mismatch on Surface & Profile. KURDAT = '/
      DATA ERRCOD(206)/'457'/,
     & ERRMSG(206)/'Date/time Mismatch on OZONEFIL Data.    Date =    '/
      DATA ERRCOD(207)/'458'/,
     & ERRMSG(207)/'Substitution made for missing ozone data.  Date = '/
      DATA ERRCOD(208)/'459'/,
     & ERRMSG(208)/'Missing ozone data; Full conversion used.  Date = '/
      DATA ERRCOD(209)/'460'/,
     & ERRMSG(209)/'Missing Hour Identified in Meteor. Data File at   '/
      DATA ERRCOD(210)/'465'/,
     & ERRMSG(210)/'Number of Profile Levels Exceeds Max:   MXPLVL =  '/
      DATA ERRCOD(211)/'470'/,
     & ERRMSG(211)/'Mixing Height Value is < or = 0.0.   KURDAT =     '/
      DATA ERRCOD(212)/'474'/,
     & ERRMSG(212)/'WS RefHt invalid (<0.001); Not msg or clm: KURDAT='/
      DATA ERRCOD(213)/'475'/,
     & ERRMSG(213)/'WS reference height is higher than 100m.  KURDAT ='/
      DATA ERRCOD(214)/'480'/,
     & ERRMSG(214)/'Less than 1yr for MULTYEAR, MAXDCONT or ANNUAL Ave'/
      DATA ERRCOD(215)/'481'/,
     & ERRMSG(215)/'Data Remaining After End of Year. Number of Hours='/
      DATA ERRCOD(216)/'482'/,
     & ERRMSG(216)/'Too many years modeled for 24h-PM25 1h-NO2 1h-SO2:'/
      DATA ERRCOD(217)/'483'/,
     & ERRMSG(217)/'User Start Date is Earlier Than Start of Met File '/
      DATA ERRCOD(218)/'484'/,
     & ERRMSG(218)/'Restart Date < STARTEND date or start of met file '/
      DATA ERRCOD(219)/'485'/,
     & ERRMSG(219)/'MULTYR DataGap; Restart Date < STARTEND or MetFile'/
      DATA ERRCOD(220)/'486'/,
     & ERRMSG(220)/'MULTYR Date Overlap; STARTEND Date < Restart Date '/
      DATA ERRCOD(221)/'487'/,
     & ERRMSG(221)/'MULTYR Date Overlap; MetFile Start < Restart Date '/
      DATA ERRCOD(222)/'488'/,
     & ERRMSG(222)/'First met HR.ne.1; ST results may not be valid for'/
      DATA ERRCOD(223)/'489'/,
     & ERRMSG(223)/'First met HR.ne.1; EV results may not be valid for'/
      DATA ERRCOD(224)/'490'/,
     & ERRMSG(224)/'Problem reading SURFFILE date for EVENTS; MNDYHR ='/
      DATA ERRCOD(225)/'491'/,
     & ERRMSG(225)/'MAXDCONT option requires 1st Hr of met data = 01; '/
      DATA ERRCOD(226)/'495'/,
     & ERRMSG(226)/'Surface met file does not include enough variables'/
      DATA ERRCOD(227)/'496'/,
     & ERRMSG(227)/'Total precipitation in SURFFILE is zero (0.0) with'/
      DATA ERRCOD(228)/'499'/,
     & ERRMSG(228)/'PRIME plume rise error; check stack parameters for'/

      DATA ERRCOD(229)/'500'/,
     & ERRMSG(229)/'Fatal Error Occurs Opening the Data File of       '/
      DATA ERRCOD(230)/'510'/,
     & ERRMSG(230)/'Fatal Error Occurs During Reading of the File of  '/
      DATA ERRCOD(231)/'520'/,
     & ERRMSG(231)/'Fatal Error Occurs During Writing to the File of  '/
      DATA ERRCOD(232)/'530'/,
     & ERRMSG(232)/'CAUTION! Met Station ID Mismatch with SURFFILE for'/
      DATA ERRCOD(233)/'540'/,
     & ERRMSG(233)/'No RECTABLE/MAXTABLE/DAYTABLE for Average Period  '/
      DATA ERRCOD(234)/'550'/,
     & ERRMSG(234)/'File Unit/Name Conflict for the Output Option:    '/
      DATA ERRCOD(235)/'555'/,
     & ERRMSG(235)/'File Unit/Name conflict across options: GRP# AVE  '/
      DATA ERRCOD(236)/'560'/,
     & ERRMSG(236)/'User Specified File Unit .LE. 30 for OU Keyword:  '/
      DATA ERRCOD(237)/'565'/,
     & ERRMSG(237)/'Possible Conflict With Dynamically Allocated FUNIT'/
      DATA ERRCOD(238)/'570'/,
     & ERRMSG(238)/'Problem Reading Temporary Event File for Event:   '/
      DATA ERRCOD(239)/'580'/,
     & ERRMSG(239)/'End of File Reached Trying to Read the File of    '/
      DATA ERRCOD(240)/'585'/,
     & ERRMSG(240)/'Output data file for INITFILE option was not found'/
      DATA ERRCOD(241)/'590'/,
     & ERRMSG(241)/'The INITFILE filename matches a SAVEFILE filename '/
      DATA ERRCOD(242)/'592'/,
     & ERRMSG(242)/'MAXIFILE includes data past start of MULTYEAR run '/
      DATA ERRCOD(243)/'593'/,
     & ERRMSG(243)/'POSTFILE includes data past start of MULTYEAR run '/

C --- New messages:
      DATA ERRCOD(244)/'273'/,
     & ERRMSG(244)/'Range of ranks for MAXDCONT THRESH Opt is limited:'/
      DATA ERRCOD(245)/'279'/,
     & ERRMSG(245)/'Multiple URBANOPT/URBANSRC inputs not allowed for:'/
      DATA ERRCOD(246)/'498'/,
     & ERRMSG(246)/'Possible code ERROR!!! MAXDCONT mismatch for GRPID'/

      END MODULE MAIN1



c --- The following MODULE subprograms replace the *.pri "INCLUDE"
c     files formerly used for global data storage for PRIME, and the
c     /PLU/-named COMMON block used in a few subroutines.
c --- R.W. Brode, U.S. EPA/OAQPS/AQMG, 10/19/2009


      MODULE PRIME_PARAMS

c --- Formerly part of PARAMS.PRI "INCLUDE" File:
c
c----------------------------------------------------------------------
c --- PARAMETER statements                                        PRIME
c----------------------------------------------------------------------

      INTEGER, PARAMETER :: io5=7,io6=8

c --- FORTRAN I/O unit numbers:
c           IO5 - Control file                  - input  - formatted
c           IO6 - List file                     - output - formatted
c

      END MODULE PRIME_PARAMS


      MODULE PRIME_NUMPARM

c --- Formerly NUMPARM.PRI "INCLUDE" File:
c
c----------------------------------------------------------------------
c --- COMMON BLOCK /NUMPARM/ -- Parameters used in the            PRIME
c                               numerical plume rise algorithm
c----------------------------------------------------------------------
c
      INTEGER, PARAMETER :: mxnw=5000
      INTEGER, PARAMETER :: mxent=10
      INTEGER, PARAMETER :: mxentp1=mxent+1
      INTEGER :: nstep, nent
      DOUBLE PRECISION :: gravi,rgas,zmin,ds,slast,rp,
     1           alphap(mxent),betap(mxent),xcat(mxentp1)

c
c --- GENERAL PARAMETER definitions:
c          MXNW - Maximum number of downwind distances for numerical
c                 plume rise integration (should be set equal to
c                 SLAST/DS)
c         MXENT - Maximum number of perturbed entrainment coefficients
c                 entered
c
c --- FORTRAN I/O unit numbers:
c           IO5 - Control file                  - input  - formatted
c           IO6 - List file                     - output - formatted
c
c --- NUMPARM Global Variables:
c
c         GRAVI - real    - Acceleration due to gravity (m/s**2)
c          RGAS - real    - Gas constant (m**2/s**2/deg. K)
c          ZMIN - real    - Minimum plume centerline height (m)
c            DS - real    - Step size (m) in the numerical plume
c                           rise algorithm
c         NSTEP - integer - Internal save frequency of plume rise
c                           calculations (i.e., every DS*NSTEP meters)
c                           (NOTE: this the frequency with which the
c                           results are saved internally -- not that
c                           passed back from the NUMRISE routine)
c         SLAST - real    - Termination distance (m) of the plume rise
c                           calculation
c            RP - real    - Radiation coefficient (kg/m**2/deg. K**3/s)
c   ALPHAP(mxent) - real array - Perturbed entrainment coefficients
c                                (parallel)
c    BETAP(mxent) - real array - Perturbed entrainment coefficients
c                                (normal)
c   XCAT(mxentp1) - real array - Downwind distances (m) for which each
c                                perturbed entrainment coefficient
c                                (ALPHAP, BETAP) is valid (NENT+1 values
c                                for NENT entrainment coefficients).
c            NENT - integer    - Number of perturbed entrainment
c                                coefficients entered

      END MODULE PRIME_NUMPARM


      MODULE PRIME_DFSN

c --- Formerly DFSN.PRI "INCLUDE" File:
c
c----------------------------------------------------------------------
c --- COMMON BLOCK /DFSN/ -- Parameters used in the            PRIME
c                            PRIME turbulence and diffusion
c                            subroutines
c----------------------------------------------------------------------
c
      DOUBLE PRECISION :: afac,xbyrmax,wiz0,wiy0,wfz,wfy,
     &        dua_ua,xdecay,xdecayi
c
c --- DFSN Global Variables:
c
c          AFAC - real    - Diffusion transitions to ambient (with
c                           virtual source) when wake turbulence decays
c                           to AFAC*(ambient turbulence intensity) for
c                           PG classes 4, 5, and 6
c       XBYRMAX - real    - Upper limit on distance from upwind face
c                           of bldg to transition point for ambient
c                           diffusion
c       WIZ,WIY - real    - Base Turbulence intensities in wake
c       WFZ,WFY - real    - Scaling factors for sigmaz and sigmay
c        DUA_UA - real    - [Ua-U]/Ua in wake at downwind face of bldg
c                                U: average speed in wake
c                               Ua: ambient speed
c         DECAY - real    - Exponent for turbulence intensity change
c                           with distance from downwind face of bldg
c        DECAYI - real    - 1/DECAY
c     RURLIZ(6) - real    - Rural turbulence intensities in z
c     RURLIY(6) - real    - Rural turbulence intensities in y
c     URBNIZ(6) - real    - Urban turbulence intensities in z
c     URBNIY(6) - real    - Urban turbulence intensities in y
c --- Ambient turbulence intensities are inferred from Briggs (1973)
c --- "Diffusion estimation for small emissions", ATDL-106;

      END MODULE PRIME_DFSN


      MODULE PRIME_WAKEDAT

c --- Formerly WAKEDAT.PRI "INCLUDE" File:
c
c----------------------------------------------------------------------
c --- COMMON BLOCK /WAKEDAT/ -- Parameters used in the            PRIME
c                               PRIME wake and streamline
c                               subroutines
c----------------------------------------------------------------------
c
      logical lrurl
      INTEGER, PARAMETER :: mxntr=50
      INTEGER :: nwak,ncav
      DOUBLE PRECISION :: Hb,Wb,xLb,Rb,HR,xLR,xLC,
     &                    xbadj,ybadj,Ub,Urh,
     &                    xwak(mxntr),szwak(mxntr),sywak(mxntr),
     &                    drwak(mxntr),
     &                    xcav(mxntr),szcav(mxntr),sycav(mxntr),
     &                    fqcav,
     &                    vsigy, vsigz, vsigyc, vsigzc, zint

      DOUBLE PRECISION :: third  ! constant = 1/3 used in various places
                                 ! initialized for PRIME in sub. WAKINI

c --- GENERAL PARAMETER definitions:
c         MXNTR - Maximum number of downwind distances for which
c                 numerical plume rise will be reported
c
c --- WAKEDAT Global Variables:
c
c            HB - real    - Building height (m)
c            WB - real    - Building width (crosswind) - (m)
c           XLB - real    - Building length (alongwind) - (m)
c            RB - real    - Scale length (m)
c            HR - real    - Maximum cavity height (m) above ground
c           XLR - real    - Length of downwind cavity (m) from
c                           downwind face of building
c           XLC - real    - Length of roof cavity (m)
c         XBADJ - real    - Distance along the wind from the stack to
c                           the origin of the building (upwind center
c                           of effective building)
c         YBADJ - real    - Distance crosswind from the stack to
c                           the origin of the building (upwind center
c                           of effective building)
c            Ub - real    - Wind speed (m/s) at the height of bldg
c           Urh - real    - Wind speed (m/s) at release height
c
c          NWAK - integer - Number of downwind distances at which
c                           wake properties are tabulated (LE mxntr)
c   XWAK(mxntr) - real    - Downwind distance (m) from source
c  SZWAK(mxntr) - real    - Sigma-z (m) at position XWAK
c  SYWAK(mxntr) - real    - Sigma-y (m) at position XWAK
c  DRWAK(mxntr) - real    - Plume growth rate at position XWAK expressed
c                           as d/dx(plume radius) for equivalent top-hat
c          NCAV - integer - Number of downwind distances at which
c                           wake properties of cavity source are
c                           tabulated (LE mxntr)
c   XCAV(mxntr) - real    - Downwind distance (m) from primary source
c  SZCAV(mxntr) - real    - Sigma-z (m) for cavity source
c  SYCAV(mxntr) - real    - Sigma-y (m) for cavity source
c         FQCAV - real    - Fraction of plume mass captured by cavity
c         ISTAB - integer - PG stability class
c         LRURL - logical - Rural dispersion when .TRUE.
c         VSIGZ - real    - Virtual source sigma (m) for sigma-z beyond wake
c         VSIGY - real    - Virtual source sigma (m) for sigma-y beyond wake
c        VSIGZC - real    - Virtual source sigma (m) for sigma-z beyond wake
c                           for cavity source
c        VSIGYC - real    - Virtual source sigma (m) for sigma-y beyond wake
c                           for cavity source

      END MODULE PRIME_WAKEDAT


      MODULE PRIME_AMBIENT

c --- Formerly AMBIENT.PRI "INCLUDE" File:
c
c----------------------------------------------------------------------
c --- COMMON BLOCK /AMBIENT/ -- Selected met. data at one         PRIME
c                               grid cell;  used in numerical
c                               plume rise computation
c----------------------------------------------------------------------
c
      INTEGER, PARAMETER :: mxnz=100
      INTEGER, PARAMETER :: mxnzp1=mxnz+1
      INTEGER :: NZA
      DOUBLE PRECISION :: uamb(mxnz),ramb(mxnz),dedz(mxnzp1),tamb(mxnz),
     1           zfacea(mxnzp1),zgpta(mxnz),tamb0,ramb0,adia,ptgrad0

c --- GENERAL PARAMETER definitions:
c          MXNZ - Maximum number of vertical layers in
c                 the meteorological data
c
c --- COMMON BLOCK /AMBIENT/ Variables:
c
c                    NZA - integer - Number of layers
c             UAMB(mxnz) - real    - Wind speed profile (m/s) - winds
c                                    defined at cell CENTERS
c             RAMB(mxnz) - real    - Ambient air density profile
c                                    (kg/m**3) - defined at cell CENTERS
c           DEDZ(mxnzp1) - real    - Pot. temperature gradient profile
c                                    (deg. K/m) - defined at cell FACES
c             TAMB(mxnz) - real    - Temperature profile (deg .K) -
c                                    defined at cell CENTERS
c         ZFACEA(mxnzp1) - real    - Heights of layer faces (m)
c            ZGPTA(mxnz) - real    - Heights of layer centers (m)
c                  TAMB0 - real    - Surface air temperature (deg. K)
c                  RAMB0 - real    - Surface air density (kg/m**3)
c                   ADIA - real    - Dry adiabatic lapse rate (deg. K/m)
c                PTGRAD0 - real    - Minimum potential temperature lapse
c                                    rate (deg. K/m)

      END MODULE PRIME_AMBIENT


      MODULE PRIME_PLU

c --- Formerly COMMON /PLU/ in selected PRIME subroutines:
c
c----------------------------------------------------------------------
c --- Notation --- in (KG,M,S) units
c               S:      LENGTH ALONG PLUME CENTERLINE
c               X:      PLUME LOCATION (downwind from source)
c               Y:      PLUME LOCATION (crosswind from source)
c               Z:      PLUME HEIGHT
c               R:      PLUME RADIUS
c               U:      PLUME HORIZONTAL (ALONGWIND) VELOCITY COMPONENT
c               V:      PLUME CROSSWIND VELOCITY COMPONENT
c               W:      PLUME VERTICAL VELOCITY COMPONENT
c               USC:    VELOCITY ALONG PLUME CENTERLINE
c               PHI:    ANGLE BETWEEN PLUME TRAJECTORY AND GROUND
c               DEN:    PLUME DENSITY
c               TP:     PLUME TEMPERATURE
c----------------------------------------------------------------------

      DOUBLE PRECISION :: S,X,Y,Z,R,U,V,W,USC,PHI,DEN,TP

      END MODULE PRIME_PLU
