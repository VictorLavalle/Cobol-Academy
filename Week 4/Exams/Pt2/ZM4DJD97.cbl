       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZM4DJ097.
       AUTHOR. GESFOR (MEXICO), S.A. DE C.V.
       DATE-WRITTEN. JUNIO 2009.
       DATE-COMPILED.
      *
      /
      ******************************************************************
      **                                                               *
      *          " S I V A   -    M U L T I E M P R E S A S"           *
      *                                                                *
      *  OBJETIVO: GENERAR REPORTE MENSUAL DE LAS VENTAS POR DONACION  *
      *            A LA FUNDACION BANCOMER MOSTRANDO EL TOTAL DE VENTAS*
      *            POR CADA CLIENTE                                    *
      *                                                                *
      ******************************************************************
      *                                                                *
      *----------------------------------------------------------------*
      *    MARCA      AUTOR    FECHA             DESCRIPCION           *
      * ----------- --------- -------- --------------------------------*
      * FSW-1.0.0-I  XMCB085  08MAY13 SE REALIZAN LAS MODIFICACIONES DE*
      * FSW-1.0.0-F  TECNOCOM         ACUERDO A LA ODT7 SIVA 2012.     *
      *    ITS01     XMBJ056  31MAY13 MODIF PROY SIVA 2013 ODT9        *
      * SDAT-39213I  XMY0603  16AGO22 SE REALIZAN LAS MODIFICACIONES DE*
      * SDAT-39213F  ASAERK           REFORMA FISCAL (CFDI 4.0).       *
      *----------------------------------------------------------------*
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
           SELECT ARCH-PT-PARAM       ASSIGN TO UT-S-ZME097A1.
      *
           SELECT ARCH-LISTADO-CBP    ASSIGN TO UT-S-ZME097LC.
           SELECT ARCH-LISTADO-BCM    ASSIGN TO UT-S-ZME097LB.
           SELECT ARCH-SECUENC-TXT    ASSIGN TO UT-S-ZME097A2.
           SELECT ARCH-INTEF-FUNDA    ASSIGN TO UT-S-ZME097A3.
      /
      ******************************************************************
      *   DATA DIVISION                                                *
      ******************************************************************
D       DATA DIVISION.
       FILE SECTION.
      *
      ******************************************************************
      *  COPY DEL ARCHIVO DE PARAMETROS DE JCL                         *
      ******************************************************************
           COPY ZMWBV470.
      *
      ******************************************************************
      *  LISTADO DE VENTAS POR DONACION     CASA DE BOLSA              *
      ******************************************************************
       FD  ARCH-LISTADO-CBP
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0  RECORDS
           DATA RECORD   IS  REG-LISTADO-CBP
           RECORD CONTAINS 132 CHARACTERS.
       01  REG-LISTADO-CBP           PIC X(132).
      *
      ******************************************************************
      *  LISTADO DE VENTAS POR DONACION     BANCA PATRIMONIAL          *
      ******************************************************************
       FD  ARCH-LISTADO-BCM
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0  RECORDS
           DATA RECORD   IS  REG-LISTADO-BCM
           RECORD CONTAINS 132 CHARACTERS.
       01  REG-LISTADO-BCM           PIC X(132).
      *
      ******************************************************************
      *  ARCHIVO DE VENTAS POR DONACION SECUENCIAL                     *
      ******************************************************************
       FD  ARCH-SECUENC-TXT
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0  RECORDS
           DATA RECORD   IS  REG-SECUENC-TXT
           RECORD CONTAINS 376 CHARACTERS.
       01  REG-SECUENC-TXT           PIC X(376).
      *
      ******************************************************************
      *  ARCHIVO DE VENTAS POR DONACION PARA LA FUNDACION BANCOMER     *
      ******************************************************************
       FD  ARCH-INTEF-FUNDA
           LABEL RECORD IS STANDARD
           RECORDING MODE IS F
           BLOCK CONTAINS 0  RECORDS
           DATA RECORD   IS  REG-INTEF-FUNDA
      * SDAT-39213I
CIHM       RECORD CONTAINS 713 CHARACTERS.
CIHM   01  REG-INTEF-FUNDA           PIC X(713).
      * SDAT-39213F
      *
      ******************************************************************
      *  WORKING STORAGE SECTION                                       *
      ******************************************************************
       WORKING-STORAGE SECTION.
      *
      *-----------------------------------------------------------------
      *
      * SDAT-39213I
      * VARIABLES PARA LA RUTINA OBTIENE DATOS DE PARAM
       01 Z005-REGS.
           COPY    ZMWLL005.
       01    WSV-IPARAM.
             05 WSV-IPERJUR-PARAM           PIC X(02).
             05 WSV-ISUBCLAS-PARAM          PIC X(03).
             05 FILLER                      PIC X(13) VALUE SPACES.
       01    WSV-DATOSPAR.
             05 WSV-REGI-FISREC-PARAM       PIC X(03).
             05 WSV-USO-CFDI-PARAM          PIC X(03).
             05 FILLER                      PIC X(52) VALUE SPACES.
       01 WS-BANDERA-ENCONTRO               PIC X(01) VALUE SPACES.
       01 WS-A3-NOMBRE-AUX                  PIC X(285).
      * CONSTANTES
       01    RUT-ZM6LR005                   PIC X(08) VALUE 'ZM6LR005'.
       01    WSC-COT-TIT                    PIC X(02) VALUE '01'.
       01    WSC-CODSER                     PIC X(02) VALUE 'SI'.
       01    WSC-TIPCTA                     PIC X(01) VALUE 'T'.
       01    WSC-TIPDIR                     PIC X(01) VALUE 'F'.
       01    WSC-N                          PIC X(01) VALUE 'N'.
      * SDAT-39213F
      * VARIABLES PARA OBTENER EL CONSECUTIVO DEL FOLIO.
       01 RUT-ZM6RG011                 PIC  X(08) VALUE 'ZM6RG011'.
       01 W000-SIGFOLIO                PIC S9(10)V COMP-3 VALUE 0.
       01 ZG11-REGISTRO.
               COPY    ZMWBV011.
      *-----------------------------------------------------------------
      *
      *
      ******************************************************************
      *  REGISTRO DE PARAMETROS MANDADOS DESDE EL JCL                  *
      ******************************************************************
       01 WS-REG-PARAMETROS.
          05 WS-EMPRESA              PIC X(03)      VALUE SPACES.
          05 FILLER                  PIC X(93)      VALUE SPACES.
      *
      ******************************************************************
      *  REGISTRO ACTUAL DEL CURSOR DE OPERACIONES                     *
      ******************************************************************
       01 WS-REGISTRO-ACT.
          05 WS-REG-ACT-IEMPR       PIC X(03)       VALUE SPACES.
          05 WS-REG-ACT-IEMISORA    PIC X(07)       VALUE SPACES.
          05 WS-REG-ACT-ISERIE      PIC X(08)       VALUE SPACES.
          05 WS-REG-ACT-ICUPON      PIC 9(03)       VALUE ZEROS.
          05 WS-REG-ACT-ISUCCASA    PIC X(03)       VALUE SPACES.
          05 WS-REG-ACT-IPROM       PIC 9(04)       VALUE ZEROS.
          05 WS-REG-ACT-ICUENTA     PIC 9(07)       VALUE ZEROS.
          05 WS-REG-ACT-NOMBRE      PIC X(20)       VALUE SPACES.
          05 WS-REG-ACT-NAPELL1     PIC X(40)       VALUE SPACES.
          05 WS-REG-ACT-NAPELL2     PIC X(20)       VALUE SPACES.
          05 WS-REG-ACT-IRFC        PIC X(13)       VALUE SPACES.
          05 WS-REG-ACT-DCALLE      PIC X(25)       VALUE SPACES.
          05 WS-REG-ACT-DCOLON      PIC X(25)       VALUE SPACES.
          05 WS-REG-ACT-DCALLEP     PIC X(25)       VALUE SPACES.
          05 WS-REG-ACT-DCOLONP     PIC X(25)       VALUE SPACES.
          05 WS-REG-ACT-DPOBLA      PIC X(30)       VALUE SPACES.
          05 WS-REG-ACT-DESTADO     PIC X(25)       VALUE SPACES.
          05 WS-REG-ACT-DPOBLAP     PIC X(30)       VALUE SPACES.
          05 WS-REG-ACT-DESTADOP    PIC X(25)       VALUE SPACES.
          05 WS-REG-ACT-IPOS        PIC 9(05)       VALUE ZEROS.
          05 WS-REG-ACT-IPOSP       PIC 9(05)       VALUE ZEROS.
       01 WS-CANTIDAD-REG-ACT.
          05 WS-REG-ACT-TOT-VTA-CTA PIC S9(07)V      COMP-3 VALUE ZEROS.
          05 WS-REG-ACT-TOT-TIT-CTA PIC S9(13)V      COMP-3 VALUE ZEROS.
          05 WS-REG-ACT-TOT-MNT-CTA PIC S9(15)V9(02) COMP-3 VALUE ZEROS.
      *
      ******************************************************************
      *  REGISTRO ANTERIOR DEL CURSOR DE OPERACIONES                   *
      ******************************************************************
       01 WS-REGISTRO-ANT.
          05 WS-REG-ANT-IEMPR       PIC X(03)       VALUE SPACES.
          05 WS-REG-ANT-IEMISORA    PIC X(07)       VALUE SPACES.
          05 WS-REG-ANT-ISERIE      PIC X(08)       VALUE SPACES.
          05 WS-REG-ANT-ICUPON      PIC 9(03)       VALUE ZEROS.
          05 WS-REG-ANT-ISUCCASA    PIC X(03)       VALUE SPACES.
          05 WS-REG-ANT-IPROM       PIC 9(04)       VALUE ZEROS.
          05 WS-REG-ANT-ICUENTA     PIC 9(07)       VALUE ZEROS.
          05 WS-REG-ANT-NOMBRE      PIC X(20)       VALUE SPACES.
          05 WS-REG-ANT-NAPELL1     PIC X(40)       VALUE SPACES.
          05 WS-REG-ANT-NAPELL2     PIC X(20)       VALUE SPACES.
          05 WS-REG-ANT-IRFC        PIC X(13)       VALUE SPACES.
          05 WS-REG-ANT-DCALLE      PIC X(25)       VALUE SPACES.
          05 WS-REG-ANT-DCOLON      PIC X(25)       VALUE SPACES.
          05 WS-REG-ANT-DCALLEP     PIC X(25)       VALUE SPACES.
          05 WS-REG-ANT-DCOLONP     PIC X(25)       VALUE SPACES.
          05 WS-REG-ANT-DPOBLA      PIC X(30)       VALUE SPACES.
          05 WS-REG-ANT-DESTADO     PIC X(25)       VALUE SPACES.
          05 WS-REG-ANT-DPOBLAP     PIC X(30)       VALUE SPACES.
          05 WS-REG-ANT-DESTADOP    PIC X(25)       VALUE SPACES.
          05 WS-REG-ANT-IPOS        PIC 9(05)       VALUE ZEROS.
          05 WS-REG-ANT-IPOSP       PIC 9(05)       VALUE ZEROS.
       01 WS-CANTIDAD-REG-ANT.
          05 WS-REG-ANT-TOT-VTA-CTA PIC S9(07)V      COMP-3 VALUE ZEROS.
          05 WS-REG-ANT-TOT-TIT-CTA PIC S9(13)V      COMP-3 VALUE ZEROS.
          05 WS-REG-ANT-TOT-MNT-CTA PIC S9(15)V9(02) COMP-3 VALUE ZEROS.
      *
      ******************************************************************
      *  RUTINAS UTILIZADAS EN EL PROGRAMA                             *
      ******************************************************************
       01 RUT-ZM6RG042               PIC  X(08)      VALUE 'ZM6RG042'.
      *
      ******************************************************************
      *  VARIABLES UTILIZADAS EN EL PROGRAMA                           *
      ******************************************************************
       01 W000-PROG                  PIC X(08)       VALUE 'ZM4DJ097'.
       01 WS-ARC-ENCAB               PIC 9(05)       VALUE ZEROS.
       01 WS-EMP                     PIC X(03)       VALUE SPACES.
       01 WS-CUENTA-CARGO            PIC X(10)       VALUE SPACES.
       01 WS-CUENTA-ABONO            PIC X(10)       VALUE SPACES.
       01 WS-IEMPR-ANT               PIC X(03)       VALUE SPACES.
       01 WS-IEMISORA-ANT            PIC X(07)       VALUE SPACES.
       01 WS-ISERIE-ANT              PIC X(08)       VALUE SPACES.
       01 WS-TOT-REG-EMP             PIC 9(09)       VALUE ZEROS.
       01 WS-TOT-VTA-EMP             PIC 9(09)       VALUE ZEROS.
       01 WS-TOT-MNT-EMP             PIC 9(15)V9(02) VALUE ZEROS.
       01 WS-TOT-REG-EMI             PIC 9(09)       VALUE ZEROS.
       01 WS-TOT-VTA-EMI             PIC 9(09)       VALUE ZEROS.
       01 WS-TOT-MNT-EMI             PIC 9(15)V9(02) VALUE ZEROS.
       01 WS-TOT-REG-SER             PIC 9(09)       VALUE ZEROS.
       01 WS-TOT-VTA-SER             PIC 9(09)       VALUE ZEROS.
       01 WS-TOT-TIT-SER             PIC 9(13)       VALUE ZEROS.
       01 WS-TOT-MNT-SER             PIC 9(15)V9(02) VALUE ZEROS.
       01 WS-TOT-VTA-CTA             PIC 9(09)       VALUE ZEROS.
       01 WS-TOT-TIT-CTA             PIC 9(13)       VALUE ZEROS.
       01 WS-TOT-MNT-CTA             PIC 9(15)V9(02) VALUE ZEROS.
      *
       01 WS-HORA-SISTEMA.
          05 WS-HH                   PIC 9(02)       VALUE ZEROS.
          05 WS-MM                   PIC 9(02)       VALUE ZEROS.
          05 WS-SS                   PIC 9(02)       VALUE ZEROS.
          05 WS-DC                   PIC 9(02)       VALUE ZEROS.
      *
       01 WS-VARIABLES.
          05 WS-SQLCODE              PIC -999        VALUE ZEROS.
          05 WS-PROGRAMA             PIC X(08)       VALUE 'ZM4DJ097'.
          05 WS-FIN-ARCHIVO          PIC X(01)       VALUE SPACES.
      *
          05 WS-NOMBRE-CTE.
             10 WS-NOMBRE            PIC  X(020)     VALUE  SPACES.
             10 WS-PATERNO           PIC  X(040)     VALUE  SPACES.
             10 WS-MATERNO           PIC  X(020)     VALUE  SPACES.
             10 F                    PIC  X(010)     VALUE  SPACES.
      *
          05 WS-SEPARADOR            PIC  X(002)        VALUE  ', '.
      *
          05 WS-DIRECCION.
             10 WS-DIRECC-PARTE1     PIC  X(025)        VALUE  SPACES.
             10 WS-DIRECC-PARTE2     PIC  X(025)        VALUE  SPACES.
             10 F                    PIC  X(001)        VALUE  SPACES.
             10 WS-NUM-EXT-INT       PIC  X(025)        VALUE  SPACES.
             10 F                    PIC  X(014)        VALUE  SPACES.
      *
          05 WS-COLONIA.
             10 WS-COLON-PARTE1      PIC  X(025)        VALUE  SPACES.
             10 WS-COLON-PARTE2      PIC  X(025)        VALUE  SPACES.
             10 F                    PIC  X(040)        VALUE  SPACES.
      *
          05 WS-CIUDAD-ESTADO.
             10 WS-CIUDAD            PIC  X(040)        VALUE  SPACES.
             10 WS-SEP               PIC  X(002)        VALUE  SPACES.
             10 WS-ESTADO            PIC  X(040)        VALUE  SPACES.
             10 F                    PIC  X(008)        VALUE  SPACES.
      *
          05 WS-LONG                 PIC S9(009)  COMP  VALUE  +90.
          05 WS-STRING               PIC  X(90)         VALUE SPACES.
      *
          05 WS-IRFC.
             10 WS-RFC-ALFA.
                15 WS-RFC-ALFA-3     PIC  X(003)        VALUE  SPACES.
                15 WS-RFC-ALFA-4     PIC  X(001)        VALUE  SPACES.
             10 WS-RFC-NUM           PIC  9(006)        VALUE  ZEROS.
             10 WS-RFC-HOMO          PIC  X(003)        VALUE  SPACES.
      *
          05 WS-IRFC1.
             10 FILLER               PIC  X(001)        VALUE  SPACES.
             10 WS-IRFC1-OK          PIC  X(012)        VALUE  SPACES.
      *
          05 WS-F01-RFC.
             10 WS-F01-RFC-ALFA      PIC  X(004)        VALUE  SPACES.
             10 F                    PIC  X(001)        VALUE  '-'.
             10 WS-F01-RFC-NUM       PIC  X(006)        VALUE  SPACES.
             10 F                    PIC  X(001)        VALUE  '-'.
             10 WS-F01-RFC-HOMO      PIC  X(003)        VALUE  SPACES.
      *
          05 WS-F01-RFC-FBCM.
             10 WS-F01-RFC-ALFA1     PIC  X(004)        VALUE  SPACES.
             10 WS-F01-RFC-NUM1      PIC  X(006)        VALUE  SPACES.
             10 WS-F01-RFC-HOMO1     PIC  X(003)        VALUE  SPACES.
      *
      * SDAT-39213I
       01 W000-CONCAT-NF                        VALUE SPACES.
          05 W000-NF-NOMBRE            PIC X(95).
          05 W000-NF-APE1              PIC X(95).
          05 W000-NF-APE2              PIC X(95).
      *
       01 WS-VAR-AUX-NF.
          05 WS-LONG-NF              PIC S9(009)  COMP  VALUE  +285.
          05 WS-STRING-NF            PIC  X(285)        VALUE SPACES.
      * SDAT-39213F
      *
      * FSW-1.0.0-I
       01 W000-CONCATENA                        VALUE SPACES.
          05 W000-CAMPO1               PIC X(21).
          05 W000-CAMPO2               PIC X(41).
          05 W000-CAMPO3               PIC X(28).
          05 FILLER                    PIC X(30).
      *
       01 W000-CONCATENA-MOR                    VALUE SPACES.
          05 W000-CAMPO1-MOR           PIC X(40).
          05 W000-CAMPO2-MOR           PIC X(20).
          05 W000-CAMPO3-MOR           PIC X(30).
          05 FILLER                    PIC X(30).
      * FSW-1.0.0-F
      *
      *INICIA
       01 WS-CCTAINVPAT.
          05 WS-CCTAINVPAT-PROD     PIC  9(02)         VALUE ZEROS.
          05 WS-CCTAINVPAT-NCTA     PIC  9(08)         VALUE ZEROS.
       01 WS-CCTAABPIGO.
          05 WS-CCTAABPIGO-PLZA     PIC  9(03)         VALUE ZEROS.
          05 WS-CCTAABPIGO-MONE     PIC  9(01)         VALUE ZEROS.
          05 WS-CCTAABPIGO-NCTA     PIC  9(08)         VALUE ZEROS.
       01 WS-CUENTA-ECT             PIC  9(08)         VALUE ZEROS.
       01 FILLER REDEFINES WS-CUENTA-ECT.
          05 FILLER                 PIC  X(04).
          05 WS-CUENTA-ECT-4-ULT    PIC  9(04).
      *TERMINA
      *
       01 W000-FECHA-ACTUAL          PIC X(10)       VALUE '0001-01-01'.
       01 FILLER REDEFINES W000-FECHA-ACTUAL.
          05 W000-FECHA-AA           PIC 9(04).
          05 FILLER                  PIC X(01).
          05 W000-FECHA-MM           PIC 9(02).
          05 FILLER                  PIC X(01).
          05 W000-FECHA-DD           PIC 9(02).
      *
       01 W000-FECHA-INI-MES         PIC X(10)       VALUE '0001-01-01'.
       01 FILLER REDEFINES W000-FECHA-INI-MES.
          05 W000-FECHA-AA-INI       PIC 9(04).
          05 FILLER                  PIC X(01).
          05 W000-FECHA-MM-INI       PIC 9(02).
          05 FILLER                  PIC X(01).
          05 W000-FECHA-DD-INI       PIC 9(02).
      *
       01 W000-FECHA-FIN-MES         PIC X(10)       VALUE '0001-01-01'.
       01 FILLER REDEFINES W000-FECHA-FIN-MES.
          05 W000-FECHA-AA-FIN       PIC 9(04).
          05 FILLER                  PIC X(01).
          05 W000-FECHA-MM-FIN       PIC 9(02).
          05 FILLER                  PIC X(01).
          05 W000-FECHA-DD-FIN       PIC 9(02).

      *
       01 W000-HORA-FIN-MES          PIC X(08)       VALUE '00:00:00'.
       01 FILLER REDEFINES W000-HORA-FIN-MES.
          05 W000-HH-FIN-MES         PIC 9(02).
          05 FILLER                  PIC X(01).
          05 W000-MM-FIN-MES         PIC 9(02).
          05 FILLER                  PIC X(01).
          05 W000-SS-FIN-MES         PIC 9(02).

      *
      ******************************************************************
      *  REGISTRO DE LA TABLA PARAM "P05"
      ******************************************************************
       01 WS-DATOSPAR-P05            PIC  X(58).
       01 FILLER REDEFINES WS-DATOSPAR-P05.
          05 P05ESTAT                PIC  X(01).
          05 P05FECHO                PIC  X(10).
          05 P05FECVN                PIC  X(10).
          05 P05DIAVN                PIC  9(02).
          05 P05FEC48                PIC  X(10).
          05 P05DIA48                PIC  9(02).
          05 P05FEC72                PIC  X(10).
          05 P05DIA72                PIC  9(02).
          05 FILLER                  PIC  X(11).
      *
      ******************************************************************
      * VARIABLES PARA ELIMINAR ESPACIOS A LA IZQUIERDA O DERECHA
      ******************************************************************
           EXEC SQL
               INCLUDE ZMWSC014
           END-EXEC.
      *
      ******************************************************************
      *    VARIABLES DB2
      ******************************************************************
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA EMPRESA
      ******************************************************************
           EXEC SQL
               INCLUDE  ZEMPRESA
           END-EXEC.
      * SDAT-39213I
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA NUEVA ZMDT895 CREADA PARA DATOS FISCALES
      ******************************************************************
           EXEC SQL
               INCLUDE  ZZMDT895
           END-EXEC.
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA PARA OBTENER EL CP FISCAL PARA CBP.
      ******************************************************************
           EXEC SQL
               INCLUDE  ZZMDTDIR
           END-EXEC.
      * SDAT-39213F
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA PARAM
      ******************************************************************
           EXEC SQL
               INCLUDE  ZPARAM
           END-EXEC.
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA CONCEPT
      ******************************************************************
           EXEC SQL
               INCLUDE  ZCONCEPT
           END-EXEC.
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA CUENTA
      ******************************************************************
           EXEC SQL
               INCLUDE  ZCUENTA
           END-EXEC.
      *
      * FSW-1.0.0-I
      *
      ******************************************************************
      *       WORKING  DE LA TABLA DE DB2: PERJUR                      *
      ******************************************************************

           EXEC SQL
               INCLUDE ZPERJUR
           END-EXEC.
      * FSW-1.0.0-F
      *
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA CTABCO
      ******************************************************************
           EXEC SQL
               INCLUDE  ZCTABCO
           END-EXEC.
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA OPERASI
      ******************************************************************
           EXEC SQL
               INCLUDE  ZOPERASI
           END-EXEC.
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA ZMDT608
      ******************************************************************
           EXEC SQL
               INCLUDE  ZZMDT608
           END-EXEC.
      *INICIA
      ******************************************************************
      *    REGISTRO DE LA TABLA ZMDT609
      ******************************************************************
           EXEC SQL
               INCLUDE  ZZMDT609
           END-EXEC.
      *TERMINA
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA ZMDT633
      ******************************************************************
           EXEC SQL
               INCLUDE  ZZMDT633
           END-EXEC.
CIHM  *
      ******************************************************************
      *    REGISTRO DE LA TABLA ACLICTA
      ******************************************************************
           EXEC SQL
               INCLUDE  ZACLICTA
           END-EXEC.
      *
      ******************************************************************
      *    REGISTRO DE LA TABLA CTECOMP
      ******************************************************************
           EXEC SQL
               INCLUDE  ZCTECOMP
CIHM       END-EXEC.
      *
      ******************************************************************
      *    FECHAS Y HORA DE COMPILACION
      ******************************************************************
                COPY ZMWBV004.
      *
      ******************************************************************
      *    ABREVIATURAS DE LOS MESES
      ******************************************************************
                COPY ZMWBV003.
      *
      ******************************************************************
      * VARIABLES DE LA RUTINA DE FECHAS (ZM6RG042)                    *
      ******************************************************************
                COPY ZMWBV492.
      *
      ******************************************************************
      *    VARIABLES DE IMPRESION PARA LISTADO DE CLIENTES
      ******************************************************************
           EXEC  SQL
               INCLUDE  ZMWBV000
           END-EXEC.
      *
           EXEC  SQL
               INCLUDE  ZMWBV471
           END-EXEC.
      *
       01  R1-TITULO.
           05 FILLER                 PIC X(42) VALUE
             'ADMINISTRACION DE SOCIEDADES DE INVERSION '.
      *
       01  R1-04-ENCA.
           05 FILLER                 PIC X(44) VALUE
             '                      DONACIONES A LA FUNDAC'.
           05 FILLER                 PIC X(44) VALUE
             'ION A TRAVES DE VENTAS EN EL FONDO DE RESPON'.
           05 FILLER                 PIC X(44) VALUE
             'SABILIDAD SOCIAL                            '.
      *
       01  R1-ESPACIOS.
           05 FILLER                 PIC X(132) VALUE SPACES.
      *
       01  R1-05-ENCA.
           05 FILLER                 PIC X(44) VALUE
             '                       EMISORA  SERIE   CUPO'.
           05 FILLER                 PIC X(44) VALUE
             'N      +---- P  E  R  I  O  D  O ----+      '.
           05 FILLER                 PIC X(33) VALUE
             'CUENTA CARGO CONCENTRADORA:      '.
           05 R1-05-CTA-CARGO        PIC X(10).
           05 FILLER                 PIC X(01) VALUE SPACES.
      *
       01  R1-06-ENCA.
           05 FILLER                 PIC X(23) VALUE SPACES.
           05 R1-06-IEMISORA         PIC X(07).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 R1-06-ISERIE           PIC X(08).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 R1-06-ICUPON           PIC ZZ9.
           05 FILLER                 PIC X(10) VALUE '      DEL '.
           05 R1-06-DIA-INI-MES      PIC Z9.
           05 FILLER                 PIC X(01) VALUE '/'.
           05 R1-06-MES-INI-MES      PIC X(03).
           05 FILLER                 PIC X(01) VALUE '/'.
           05 R1-06-ANO-INI-MES      PIC 9(04).
           05 FILLER                 PIC X(05) VALUE '  AL '.
           05 R1-06-DIA-FIN-MES      PIC Z9.
           05 FILLER                 PIC X(01) VALUE '/'.
           05 R1-06-MES-FIN-MES      PIC X(03).
           05 FILLER                 PIC X(01) VALUE '/'.
           05 R1-06-ANO-FIN-MES      PIC 9(04).
           05 FILLER                 PIC X(06) VALUE SPACES.
           05 FILLER                 PIC X(33) VALUE
             'CUENTA ABONO FUNDACION BANCOMER: '.
           05 R1-06-CTA-ABONO        PIC X(10).
           05 FILLER                 PIC X(01) VALUE SPACES.
      *
       01  R1-07-ENCA.
           05 FILLER                 PIC X(132) VALUE ALL '-'.
      *
       01  R1-08-ENCA.
           05 FILLER                 PIC X(44) VALUE
      *ITS01-I
      *      ' EMP SUC PROM CTA.PATRIM NOMBRE DEL CLIENTE '.
             ' SUC PROM CTA.PATRIM NOMBRE DEL CLIENTE     '.
      *ITS01-F
           05 FILLER                 PIC X(44) VALUE
             '                                            '.
           05 FILLER                 PIC X(44) VALUE
             'TOT.VTAS.      TITULOS       MONTO DONADO   '.
      *
       01  R1-01-DETA.
      *ITS01-I
      *    05 FILLER                 PIC X(01) VALUE SPACES.
      *    05 R1-01-DET-IEMPR        PIC X(03).
      *ITS01-F
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-01-DET-ISUCCASA     PIC X(03).
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-01-DET-IPROM        PIC 9999.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-CCTAINVPAT.
              10 R1-01-DET-CCTAINVPAT PIC X(10).
           05 FILLER REDEFINES R1-CCTAINVPAT.
              10 R1-01-DET-BLANCOS   PIC XXX.
              10 R1-01-DET-ICUENTA   PIC 9999999.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-01-DET-NOMBRE       PIC X(64).
      *ITS01-I
      *    05 FILLER                 PIC X(01) VALUE SPACES.
           05 FILLER                 PIC X(05) VALUE SPACES.
      *ITS01-F
           05 R1-01-DET-TOT-VTAS     PIC ZZZZ9.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-01-DET-TIT-DONAC    PIC ZZZZZZ,ZZZ,ZZ9.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-01-DET-MNT-DONAC    PIC ZZZZZZ,ZZZ,ZZZ,ZZ9.99.
      *
       01  R1-02-DETA.
           05 FILLER                 PIC X(14) VALUE SPACES.
           05 FILLER                 PIC X(11) VALUE 'DIRECCION: '.
           05 R1-02-DET-DIRECCION    PIC X(64).
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 FILLER                 PIC X(05) VALUE 'RFC: '.
           05 R1-02-DET-IRFC         PIC X(15).
           05 FILLER                 PIC X(22) VALUE SPACES.
      *
       01  R1-03-DETA.
           05 FILLER                 PIC X(14) VALUE SPACES.
           05 FILLER                 PIC X(05) VALUE 'C.P. '.
           05 R1-03-DET-IPOS         PIC 99999.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-03-DET-COLONIA      PIC X(30).
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-03-DET-POBLACION    PIC X(33).
           05 FILLER                 PIC X(43) VALUE SPACES.
      *
       01  R1-01-TOTALES.
           05 FILLER                 PIC X(49) VALUE SPACES.
           05 FILLER                 PIC X(32) VALUE
              '*** TOTAL SERIE ***   CLIENTES: '.
           05 R1-01-TOT-REG-SER      PIC ZZZZZ9.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 R1-01-TOT-VTA-SER      PIC ZZZZZ9.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-01-TOT-TIT-SER      PIC ZZZZZZ,ZZZ,ZZ9.
           05 FILLER                 PIC X(01) VALUE SPACES.
           05 R1-01-TOT-MNT-SER      PIC ZZZZZZ,ZZZ,ZZZ,ZZ9.99.
      *
       01  R1-02-TOTALES.
           05 FILLER                 PIC X(49) VALUE SPACES.
           05 FILLER                 PIC X(32) VALUE
              '*** TOTAL FONDO ***   CLIENTES: '.
           05 R1-02-TOT-REG-EMI      PIC ZZZZZ9.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 R1-02-TOT-VTA-EMI      PIC ZZZZZ9.
           05 FILLER                 PIC X(16) VALUE SPACES.
           05 R1-02-TOT-MNT-EMI      PIC ZZZZZZ,ZZZ,ZZZ,ZZ9.99.
      *
       01  R1-03-TOTALES.
           05 FILLER                 PIC X(49) VALUE SPACES.
           05 FILLER                 PIC X(32) VALUE
              '*** TOTAL EMPR. ***   CLIENTES: '.
           05 R1-03-TOT-REG-EMP      PIC ZZZZZ9.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 R1-03-TOT-VTA-EMP      PIC ZZZZZ9.
           05 FILLER                 PIC X(16) VALUE SPACES.
           05 R1-03-TOT-MNT-EMP      PIC ZZZZZZ,ZZZ,ZZZ,ZZ9.99.

      *
      ******************************************************************
      *    ARCHIVO SECUENCIAL PARA ABRIR DESPUES EN EXCEL
      ******************************************************************
       01  A1-ENCABEZADO.
           05 FILLER                 PIC X(11) VALUE 'FEC. INICIO'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(11) VALUE 'FEC. FINAL '.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(10) VALUE 'CTA. CARGO'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(10) VALUE 'CTA. ABONO'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(07) VALUE 'EMISORA'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(08) VALUE 'SERIE   '.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(05) VALUE 'CUPON'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(07) VALUE 'EMPRESA'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(08) VALUE 'SUCURSAL'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(08) VALUE 'PROMOTOR'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(10) VALUE 'CTA.PATRIM'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(66) VALUE 'MOMBRE CLIENTE'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(15) VALUE 'R.F.C.'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(66) VALUE 'DIRECCION'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(30) VALUE 'COLONIA  '.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(33) VALUE 'POBLACION'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(07) VALUE 'CPOSTAL'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(09) VALUE 'TOT.VTAS.'.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(14) VALUE 'TIT. DONATIVO '.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(21) VALUE 'MONTO DONATIVO '.
           05 FILLER                 PIC X(01) VALUE ';'.
      *
      ******************************************************************
      *    ARCHIVO SECUENCIAL PARA ABRIR DESPUES EN EXCEL
      ******************************************************************
       01  A1-DETALLE.
           05 A1-DIA-INI-MES         PIC Z9.
           05 FILLER                 PIC X(01) VALUE '/'.
           05 A1-MES-INI-MES         PIC X(03).
           05 FILLER                 PIC X(01) VALUE '/'.
           05 A1-ANO-INI-MES         PIC 9(04).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-DIA-FIN-MES         PIC Z9.
           05 FILLER                 PIC X(01) VALUE '/'.
           05 A1-MES-FIN-MES         PIC X(03).
           05 FILLER                 PIC X(01) VALUE '/'.
           05 A1-ANO-FIN-MES         PIC 9(04).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-CTA-CARGO           PIC X(10).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-CTA-ABONO           PIC X(10).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-IEMISORA            PIC X(07).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-ISERIE              PIC X(08).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 A1-ICUPON              PIC ZZ9.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-IEMPR               PIC X(03).
           05 FILLER                 PIC X(04) VALUE SPACES.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-ISUCCASA            PIC X(03).
           05 FILLER                 PIC X(05) VALUE SPACES.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(04) VALUE SPACES.
           05 A1-IPROM               PIC 9999.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-CCTAINVPAT          PIC X(10).
           05 FILLER REDEFINES A1-CCTAINVPAT.
              10 A1-BLANCOS          PIC XXX.
              10 A1-ICUENTA          PIC 9999999.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-NOMBRE              PIC X(66).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-IRFC                PIC X(15).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-DIRECCION           PIC X(66).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-COLONIA             PIC X(30).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-POBLACION           PIC X(33).
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-IPOS                PIC 9(05).
           05 FILLER                 PIC X(02) VALUE SPACES.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 FILLER                 PIC X(04) VALUE SPACES.
           05 A1-TOT-VTAS            PIC ZZZZ9.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-TIT-DONAC           PIC ZZZZZZ,ZZZ,ZZ9.
           05 FILLER                 PIC X(01) VALUE ';'.
           05 A1-MNT-DONAC           PIC ZZZZZZ,ZZZ,ZZZ,ZZ9.99.
           05 FILLER                 PIC X(01) VALUE ';'.
      *
      ******************************************************************
      *    ARCHIVO SECUENCIAL INTERFAZ HACIA LA FUNDACION BANCOMER
      ******************************************************************
       01  A3-DETALLE.
           05 A3-TIPO-REG            PIC X(01).
           05 A3-APLICACION          PIC X(04).
           05 A3-FILLER1-CEROS       PIC 9(10).
           05 A3-CCTAINVPAT          PIC X(10).
           05 A3-FILLER2-CEROS       PIC 9(11).
           05 A3-NUM-OPERAC-IREF     PIC 9(09).
      * SDAT-39213I
           05 A3-NOMBRE              PIC X(285).
      * SDAT-39213F
           05 A3-IRFC                PIC X(15).
           05 A3-MNT-DONAC           PIC 9(13).9(02).
           05 A3-FECHA-FIN-MES       PIC X(10).
           05 A3-HORA-FIN-MES        PIC X(08).
           05 A3-DIRECCION           PIC X(50).
           05 A3-NUMERO-EXTERIOR     PIC X(09).
           05 A3-NUMERO-INTERIOR     PIC X(09).
           05 A3-COLONIA             PIC X(40).
           05 A3-POBLACION           PIC X(40).
           05 A3-ESTADO              PIC X(40).
           05 A3-PAIS                PIC X(30).
           05 A3-COD-POSTAL          PIC 9(05).
           05 A3-ZONA-DE-REPARTO     PIC X(03).
      *INICIA
      **** 05 A3-FILLER              PIC X(50).
           05 A3-METODO-DE-PAGO      PIC X(02).
           05 A3-NUM-CTA-ULT-4-DIG   PIC 9(04).
           05 A3-FILLER              PIC X(44).
      *TERMINA
CIHM       05 A3-EMAIL               PIC X(50).
      * SDAT-39213I
           05 A3-REG-FISCAL          PIC 9(03).
           05 A3-CP-FISCAL           PIC 9(05).
      * SDAT-39213F
      *
       01  T3-TOTALES.
           05 T3-TIPO-REG            PIC X(01).
           05 T3-APLICACION          PIC X(04).
           05 T3-TOTAL-DEPOSITOS     PIC 9(07).
           05 T3-TOTAL-MONTO-DON     PIC 9(13).9(02).
           05 T3-FILLER              PIC X(442).
      *
      ******************************************************************
      *    DECLARACION DE CURSORES
      ******************************************************************

           EXEC SQL
             DECLARE C100-OPERASI CURSOR FOR
               SELECT T1.IEMPR,              T1.ISUCCASA,
                      VALUE(T1.IPROM,0),
                      T1.ICUENTA,            VALUE(T1.NOMBRE,' '),
                      T1.NAPELL1,            VALUE(T1.NAPELL2,' '),
                      VALUE(T1.IRFC,' '),
                      VALUE(T1.DCALLE,' '),  VALUE(T1.DCOLON,' '),
                      VALUE(T1.DCALLEP,' '), VALUE(T1.DCOLONP,' '),
                      VALUE(T1.DPOBLA,' '),  VALUE(T1.DESTADO,' '),
                      VALUE(T1.DPOBLAP,' '), VALUE(T1.DESTADOP,' '),
                      VALUE(T1.IPOS,0),      VALUE(T1.IPOSP,0),
                      VALUE(T2.CANT2,0),
                      T2.CANT1,
                      VALUE(T3.IEMISORA,' '),
                      VALUE(T3.ISERIE,' '),
                      VALUE(T3.ICUPON,0)
      *
      * FSW-1.0.0-I
                     ,VALUE(T1.IPERJUR,' '),
                      T1.ISUBCLAS,
                      T1.IEMPR
      * FSW-1.0.0-F
      *
                 FROM CUENTA  T1
                    , OPERASI T2
                    , CONCEPT T3
                WHERE T1.ICUENTA   = T2.ICUENTA1
                  AND T1.IEMPR     = :WS-EMP
                  AND T2.ICONCEP2  = T3.ICONCEPT
                  AND T2.SESTATUS <> 'B'
                  AND T2.IUSUARIO IN ('ZM4DJ094', 'ZM4DJ095')
                  AND T2.FOPERA BETWEEN :W000-FECHA-INI-MES
                                    AND :W000-FECHA-FIN-MES
            UNION ALL
               SELECT T1.IEMPR,              T1.ISUCCASA,
                      VALUE(T1.IPROM,0),
                      T1.ICUENTA,            VALUE(T1.NOMBRE,' '),
                      T1.NAPELL1,            VALUE(T1.NAPELL2,' '),
                      VALUE(T1.IRFC,' '),
                      VALUE(T1.DCALLE,' '),  VALUE(T1.DCOLON,' '),
                      VALUE(T1.DCALLEP,' '), VALUE(T1.DCOLONP,' '),
                      VALUE(T1.DPOBLA,' '),  VALUE(T1.DESTADO,' '),
                      VALUE(T1.DPOBLAP,' '), VALUE(T1.DESTADOP,' '),
                      VALUE(T1.IPOS,0),      VALUE(T1.IPOSP,0),
                      VALUE(T2.CANT2,0),
                      T2.CANT1,
                      VALUE(T3.IEMISORA,' '),
                      VALUE(T3.ISERIE,' '),
                      VALUE(T3.ICUPON,0)
      *
      * FSW-1.0.0-I
                     ,VALUE(T1.IPERJUR,' '),
                      T1.ISUBCLAS,
                      T1.IEMPR
      * FSW-1.0.0-F
      *
                 FROM CUENTA  T1
                    , OPERA   T2
                    , CONCEPT T3
                WHERE T1.ICUENTA   = T2.ICUENTA1
                  AND T1.IEMPR     = :WS-EMP
                  AND T2.ICONCEP2  = T3.ICONCEPT
                  AND T2.SESTATUS <> 'B'
                  AND T2.IUSUARIO IN ('ZM4DJ094', 'ZM4DJ095')
                  AND T2.FOPERA BETWEEN :W000-FECHA-INI-MES
                                    AND :W000-FECHA-FIN-MES
                ORDER BY 1,           21,          22,
                         2,           3,           4
      *****     ORDER BY T1.IEMPR,    T3.IEMISORA, T3.ISERIE,
      *****              T1.ISUCCASA, T1.IPROM,    T1.ICUENTA
                 WITH UR
           END-EXEC.
      *
      /
      *
      ******************************************************************
      *  PROCEDURE DIVISION
      ******************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 002-OBT-FECHA-P05.
           PERFORM 005-ABRE-ARCHIVOS.
      *
      **** OBTIENE REPORTE DE POSICIONES DE LA EMPR. 'CBP' CASA DE BOLSA
           MOVE 'CBP'                   TO WS-EMP
           PERFORM 007-LIMPIA-VARIABLES.
           PERFORM 008-OBT-CTA-CARGO-ABONO.
           MOVE W000-PROG               TO WS-CTRL-D-PROGRAMA
           MOVE WS-EMP                  TO WS-CTRL-D-EMPRESA
           MOVE SPACES                  TO WS-CTRL-D-SUCURSAL
           MOVE SPACES                  TO WS-CTRL-D-PROMOTOR-A
           PERFORM 9800-OBT-CASILLERO
           MOVE WS-CTRL-D-CASILLERO     TO R1-01-CVE-CTLD
           MOVE WS-EMP                  TO R1-CVE-EMP.
      *
           PERFORM 040-ABRE-CURSOR.
           PERFORM 050-LEE-CURSOR.
           PERFORM UNTIL WS-FIN-ARCHIVO = 'S'
              PERFORM 010-CHECA-CORTE-EMP-EMI-SER
              PERFORM 020-ARMA-DETALLE
              IF WS-CUENTA-CARGO NOT = SPACES AND
                 WS-CUENTA-ABONO NOT = SPACES
                 PERFORM 030-REALIZA-CARGO-ABONO
              END-IF
              PERFORM 050-LEE-CURSOR
           END-PERFORM.
           PERFORM 010-CHECA-CORTE-EMP-EMI-SER.
           PERFORM 060-CIERRA-CURSOR.
      *
      **** OBTIENE REPORTE DE POSICIONES DE LA EMPR. 'BCM' B. PATRIM.
           MOVE 'BCM'                   TO WS-EMP
           PERFORM 007-LIMPIA-VARIABLES.
           PERFORM 008-OBT-CTA-CARGO-ABONO.
           MOVE W000-PROG               TO WS-CTRL-D-PROGRAMA
           MOVE WS-EMP                  TO WS-CTRL-D-EMPRESA
           MOVE SPACES                  TO WS-CTRL-D-SUCURSAL
           MOVE SPACES                  TO WS-CTRL-D-PROMOTOR-A
           PERFORM 9800-OBT-CASILLERO
           MOVE WS-CTRL-D-CASILLERO     TO R1-01-CVE-CTLD
           MOVE WS-EMP                  TO R1-CVE-EMP.
      *
           PERFORM 040-ABRE-CURSOR.
           PERFORM 050-LEE-CURSOR.
           PERFORM UNTIL WS-FIN-ARCHIVO = 'S'
              PERFORM 010-CHECA-CORTE-EMP-EMI-SER
              PERFORM 020-ARMA-DETALLE
              IF WS-CUENTA-CARGO NOT = SPACES AND
                 WS-CUENTA-ABONO NOT = SPACES
                 PERFORM 030-REALIZA-CARGO-ABONO
              END-IF
              PERFORM 050-LEE-CURSOR
           END-PERFORM.
           PERFORM 010-CHECA-CORTE-EMP-EMI-SER.
           PERFORM 060-CIERRA-CURSOR.
      *
           PERFORM 080-CIERRA-ARCHIVOS.
           PERFORM 999-TERMINA.
      /
      *
      ******************************************************************
      *    VALIDA QUE EXISTE EL EVENTO CORRESPONDIENTE
      ******************************************************************
       002-OBT-FECHA-P05.
           MOVE SPACES                  TO WS-DATOSPAR-P05.
           EXEC SQL
                SELECT DATOSPAR
                  INTO :WS-DATOSPAR-P05
                  FROM PARAM
                 WHERE ITIPOPAR = 'P05'
                   AND IPARAM   = ' '
           END-EXEC.
           IF SQLCODE = ZEROS
              MOVE P05FECHO             TO W000-FECHA-ACTUAL
      *
              MOVE W000-FECHA-ACTUAL    TO W000-FECHA-INI-MES
              MOVE 01                   TO W000-FECHA-DD-INI
      *
              INITIALIZE                   AREA-ENLACE-ZMWBV492
              MOVE W000-FECHA-ACTUAL    TO FECHA-INICIAL-ZMWBV492
              MOVE 01                   TO DD-INICIAL-ZMWBV492
              ADD  01                   TO MM-INICIAL-ZMWBV492
              IF MM-INICIAL-ZMWBV492 > 12
                 MOVE 01                TO MM-INICIAL-ZMWBV492
                 ADD 1                  TO AA-INICIAL-ZMWBV492
              END-IF
              MOVE -1                   TO DIAS-A-CONTAR-ZMWBV492
              MOVE SPACES               TO FECHA-FINAL-ZMWBV492
              MOVE SPACES               TO IND-OPCION-ZMWBV492
              CALL RUT-ZM6RG042      USING AREA-ENLACE-ZMWBV492
              MOVE FECHA-FINAL-ZMWBV492 TO W000-FECHA-FIN-MES
           ELSE
              DISPLAY 'ERROR AL LEER  PARAM ("P05") '
              MOVE SQLCODE              TO WS-SQLCODE
              DISPLAY 'SQLCODE ' WS-SQLCODE
              DISPLAY 'SE TERMINA EL PROGRAMA                '
              PERFORM 999-ABORTA
           END-IF.
       002-FIN.
           EXIT.
      *
      ******************************************************************
      *   ABRE ARCHIVOS LOS LISTADOS DE CASA DE BOLSA Y BANCA PATRIM.
      ******************************************************************
       005-ABRE-ARCHIVOS.
           OPEN OUTPUT ARCH-LISTADO-CBP
                       ARCH-LISTADO-BCM
                       ARCH-SECUENC-TXT
                       ARCH-INTEF-FUNDA.

      *
           MOVE ZEROS                   TO WS-ARC-ENCAB.
      *
           OPEN INPUT  ARCH-PT-PARAM.
      *
           READ ARCH-PT-PARAM         INTO WS-REG-PARAMETROS
                AT END MOVE 'S'         TO WS-FIN-ARCHIVO.
           IF WS-FIN-ARCHIVO = 'S'
              DISPLAY 'ARCHIVO DE PARAMETROS DE EMPRESA VACIO'
              DISPLAY 'SE TERMINA EL PROGRAMA                '
              PERFORM 999-ABORTA
           END-IF.
           IF WS-EMPRESA = SPACES
              DISPLAY 'PARAMETRO DE EMPRESA EN BLANCO        '
              DISPLAY 'SE TERMINA EL PROGRAMA                '
              PERFORM 999-ABORTA
           END-IF.
           MOVE WS-EMPRESA              TO WS-EMP.
      *
           CLOSE       ARCH-PT-PARAM.
       005-FIN.
           EXIT.
      *
      ******************************************************************
      *   INICIALIZA LAS VARIABLES REQUERIDAS PARA EL REPORTE
      ******************************************************************
       007-LIMPIA-VARIABLES.
           MOVE SPACES                  TO WS-IEMPR-ANT.
           MOVE SPACES                  TO WS-IEMISORA-ANT.
           MOVE SPACES                  TO WS-ISERIE-ANT.
           MOVE SPACES                  TO IEMPR    OF DCLCUENTA.
           MOVE SPACES                  TO IEMISORA OF DCLCONCEPT.
           MOVE SPACES                  TO ISERIE   OF DCLCONCEPT.
           MOVE ZEROS                   TO WS-TOT-REG-EMP.
           MOVE ZEROS                   TO WS-TOT-VTA-EMP.
           MOVE ZEROS                   TO WS-TOT-MNT-EMP.
           MOVE ZEROS                   TO WS-TOT-REG-EMI.
           MOVE ZEROS                   TO WS-TOT-VTA-EMI.
           MOVE ZEROS                   TO WS-TOT-MNT-EMI.
           MOVE ZEROS                   TO WS-TOT-REG-SER.
           MOVE ZEROS                   TO WS-TOT-VTA-SER.
           MOVE ZEROS                   TO WS-TOT-TIT-SER.
           MOVE ZEROS                   TO WS-TOT-MNT-SER.
           MOVE ZEROS                   TO R1-NUM-HOJA.
       007-FIN.
           EXIT.
      *
      ******************************************************************
      *    OBTIENE LA CUENTA CONCENTRADORA PUENTE PARA CARGO DE LAS    *
      *    DONACIONES EFECTUADAS POR EL CLIENTE Y LA CUENTA DE ABONO   *
      *    CORRESPONDIENTE A LA CUENTA DE CHEQUES DE LA FUNDACION      *
      *    BANCOMER                                                    *
      ******************************************************************
       008-OBT-CTA-CARGO-ABONO.
           MOVE SPACES                  TO WS-CUENTA-CARGO.
           MOVE WS-EMP                  TO IPARAM   OF DCLPARAM.
           MOVE SPACES                  TO DATOSPAR OF DCLPARAM.
           EXEC SQL
                SELECT DATOSPAR
                  INTO :DCLPARAM.DATOSPAR
                  FROM PARAM
                 WHERE ITIPOPAR = 'FBM'
                   AND IPARAM   = :DCLPARAM.IPARAM
           END-EXEC.
           IF SQLCODE = ZEROS
              IF DATOSPAR OF DCLPARAM (01:10) IS NUMERIC
              OR DATOSPAR OF DCLPARAM (01:10)  = SPACES
                 MOVE DATOSPAR OF DCLPARAM (01:10)
                                        TO WS-CUENTA-CARGO
              ELSE
                 DISPLAY 'ERROR AL LEER  PARAM ("FBM") '
                 DISPLAY 'CTA DE CARGO NO VALIDA "'
                         DATOSPAR OF DCLPARAM (01:10) '"'
                 MOVE SQLCODE           TO WS-SQLCODE
                 DISPLAY 'SQLCODE ' WS-SQLCODE
                 DISPLAY 'SE TERMINA EL PROGRAMA                '
                 PERFORM 999-ABORTA
              END-IF
           END-IF.
      *
           MOVE SPACES                  TO WS-CUENTA-ABONO.
           MOVE 'FBM'                   TO IPARAM   OF DCLPARAM.
           MOVE SPACES                  TO DATOSPAR OF DCLPARAM.
           EXEC SQL
                SELECT DATOSPAR
                  INTO :DCLPARAM.DATOSPAR
                  FROM PARAM
                 WHERE ITIPOPAR = 'FBM'
                   AND IPARAM   = :DCLPARAM.IPARAM
           END-EXEC.
           IF SQLCODE = ZEROS
              IF DATOSPAR OF DCLPARAM (01:10) IS NUMERIC
              OR DATOSPAR OF DCLPARAM (01:10)  = SPACES
                 MOVE DATOSPAR OF DCLPARAM (01:10)
                                        TO WS-CUENTA-ABONO
              ELSE
                 DISPLAY 'ERROR AL LEER  PARAM ("FBM") '
                 DISPLAY 'CTA DE ABONO NO VALIDA "'
                         DATOSPAR OF DCLPARAM (01:10) '"'
                 MOVE SQLCODE           TO WS-SQLCODE
                 DISPLAY 'SQLCODE ' WS-SQLCODE
                 DISPLAY 'SE TERMINA EL PROGRAMA                '
                 PERFORM 999-ABORTA
              END-IF
           END-IF.
       008-FIN.
           EXIT.
      *
      ******************************************************************
      *   CHECA SI EXISTE CORTE DE EMPRESA, EMISORA, SERIE
      ******************************************************************
       010-CHECA-CORTE-EMP-EMI-SER.
           IF IEMPR    OF DCLCUENTA  NOT = WS-IEMPR-ANT
           OR IEMISORA OF DCLCONCEPT NOT = WS-IEMISORA-ANT
           OR ISERIE   OF DCLCONCEPT NOT = WS-ISERIE-ANT
              IF WS-TOT-REG-SER NOT = ZEROS
                 MOVE WS-TOT-REG-SER         TO R1-01-TOT-REG-SER
                 MOVE WS-TOT-VTA-SER         TO R1-01-TOT-VTA-SER
                 MOVE WS-TOT-TIT-SER         TO R1-01-TOT-TIT-SER
                 MOVE WS-TOT-MNT-SER         TO R1-01-TOT-MNT-SER
                 IF WS-EMP = 'CBP'
                    WRITE REG-LISTADO-CBP  FROM R1-ESPACIOS
                    WRITE REG-LISTADO-CBP  FROM R1-01-TOTALES
                    ADD 2                    TO R1-NUM-LIN
                 ELSE
                    WRITE REG-LISTADO-BCM  FROM R1-ESPACIOS
                    WRITE REG-LISTADO-BCM  FROM R1-01-TOTALES
                    ADD 2                    TO R1-NUM-LIN
                 END-IF
                 MOVE ZEROS                  TO WS-TOT-REG-SER
                 MOVE ZEROS                  TO WS-TOT-VTA-SER
                 MOVE ZEROS                  TO WS-TOT-TIT-SER
                 MOVE ZEROS                  TO WS-TOT-MNT-SER
              END-IF
           END-IF.
      *
           IF IEMPR    OF DCLCUENTA  NOT = WS-IEMPR-ANT
           OR IEMISORA OF DCLCONCEPT NOT = WS-IEMISORA-ANT
              IF WS-TOT-REG-EMI NOT = ZEROS
                 MOVE WS-TOT-REG-EMI         TO R1-02-TOT-REG-EMI
                 MOVE WS-TOT-VTA-EMI         TO R1-02-TOT-VTA-EMI
                 MOVE WS-TOT-MNT-EMI         TO R1-02-TOT-MNT-EMI
                 IF WS-EMP = 'CBP'
                    WRITE REG-LISTADO-CBP  FROM R1-02-TOTALES
                    ADD 1                    TO R1-NUM-LIN
                 ELSE
                    WRITE REG-LISTADO-BCM  FROM R1-02-TOTALES
                    ADD 1                    TO R1-NUM-LIN
                 END-IF
                 MOVE ZEROS                  TO WS-TOT-REG-EMI
                 MOVE ZEROS                  TO WS-TOT-VTA-EMI
                 MOVE ZEROS                  TO WS-TOT-MNT-EMI
              END-IF
           END-IF.
      *
           IF IEMPR    OF DCLCUENTA NOT = WS-IEMPR-ANT
              IF WS-TOT-REG-EMP NOT = ZEROS
                 MOVE WS-TOT-REG-EMP         TO R1-03-TOT-REG-EMP
                 MOVE WS-TOT-VTA-EMP         TO R1-03-TOT-VTA-EMP
                 MOVE WS-TOT-MNT-EMP         TO R1-03-TOT-MNT-EMP
                 IF WS-EMP = 'CBP'
                    WRITE REG-LISTADO-CBP  FROM R1-03-TOTALES
                    WRITE REG-LISTADO-CBP  FROM R1-ESPACIOS
                    ADD 2                    TO R1-NUM-LIN
                 ELSE
                    WRITE REG-LISTADO-BCM  FROM R1-03-TOTALES
                    WRITE REG-LISTADO-BCM  FROM R1-ESPACIOS
                    ADD 2                    TO R1-NUM-LIN
                 END-IF
      *
                 INITIALIZE                     T3-TOTALES
                 MOVE '2'                    TO T3-TIPO-REG
                 IF WS-EMP = 'BCM'
                    MOVE 'MUV '              TO T3-APLICACION
                 ELSE
                    MOVE 'CBP '              TO T3-APLICACION
                 END-IF
                 MOVE WS-TOT-REG-EMP         TO T3-TOTAL-DEPOSITOS
                 MOVE WS-TOT-MNT-EMP         TO T3-TOTAL-MONTO-DON
                 MOVE SPACES                 TO T3-FILLER
                 WRITE REG-INTEF-FUNDA     FROM T3-TOTALES
      *
                 MOVE ZEROS                  TO WS-TOT-REG-EMP
                 MOVE ZEROS                  TO WS-TOT-VTA-EMP
                 MOVE ZEROS                  TO WS-TOT-MNT-EMP
              END-IF
           END-IF.
      *
           IF IEMISORA OF DCLCONCEPT NOT = WS-IEMISORA-ANT
           OR ISERIE   OF DCLCONCEPT NOT = WS-ISERIE-ANT
           OR IEMPR    OF DCLCUENTA  NOT = WS-IEMPR-ANT
              IF IEMPR OF DCLCUENTA  NOT = HIGH-VALUES
                 PERFORM 160-ENCABEZADO
              ELSE
                 IF WS-IEMPR-ANT = SPACES
                    MOVE SPACES         TO IEMISORA OF DCLCONCEPT
                    MOVE SPACES         TO ISERIE   OF DCLCONCEPT
                    MOVE ZEROS          TO ICUPON   OF DCLCONCEPT
                    PERFORM 160-ENCABEZADO
                 END-IF
              END-IF
           END-IF.
      *
           MOVE ISERIE   OF DCLCONCEPT      TO WS-ISERIE-ANT.
           MOVE IEMISORA OF DCLCONCEPT      TO WS-IEMISORA-ANT.
           MOVE IEMPR    OF DCLCUENTA       TO WS-IEMPR-ANT.
       010-FIN.
           EXIT.
      *
      ******************************************************************
      *   ARMA EL DETALLE DE UN REGISTRO A IMPRIMIR
      ******************************************************************
       020-ARMA-DETALLE.
           MOVE W000-FECHA-DD-INI           TO A1-DIA-INI-MES
           MOVE T999-ABREV-MES(W000-FECHA-MM-INI)
                                            TO A1-MES-INI-MES
           MOVE W000-FECHA-AA-INI           TO A1-ANO-INI-MES
      *
           MOVE W000-FECHA-DD-FIN           TO A1-DIA-FIN-MES
           MOVE T999-ABREV-MES(W000-FECHA-MM-FIN)
                                            TO A1-MES-FIN-MES
           MOVE W000-FECHA-AA-FIN           TO A1-ANO-FIN-MES
      *
           MOVE WS-CUENTA-CARGO             TO A1-CTA-CARGO
           MOVE WS-CUENTA-ABONO             TO A1-CTA-ABONO
      *
           MOVE IEMISORA OF DCLCONCEPT      TO A1-IEMISORA
           MOVE ISERIE   OF DCLCONCEPT      TO A1-ISERIE
           MOVE ICUPON   OF DCLCONCEPT      TO A1-ICUPON
      *
      *ITS01-I
      *    MOVE IEMPR    OF DCLCUENTA       TO R1-01-DET-IEMPR
      *                                        A1-IEMPR
           MOVE IEMPR    OF DCLCUENTA       TO A1-IEMPR
      *ITS01-F
           MOVE ISUCCASA OF DCLCUENTA       TO R1-01-DET-ISUCCASA
                                               A1-ISUCCASA
           MOVE IPROM    OF DCLCUENTA       TO R1-01-DET-IPROM
                                               A1-IPROM
      *
           MOVE SPACES                      TO ZM608-CCTAINVPAT
           MOVE ICUENTA  OF DCLCUENTA       TO ZM608-NICUENTA
           EXEC SQL
              SELECT  ZM608_CCTAINVPAT
                INTO :ZM608-CCTAINVPAT
                FROM ZMDT608
               WHERE ZM608_NICUENTA = :ZM608-NICUENTA
           END-EXEC.
           IF ZM608-CCTAINVPAT = SPACES
           OR ZM608-CCTAINVPAT = '0000000000'
           OR ZM608-CCTAINVPAT IS NOT NUMERIC
              MOVE SPACES                   TO R1-01-DET-BLANCOS
                                               A1-BLANCOS
              MOVE ICUENTA  OF DCLCUENTA    TO R1-01-DET-ICUENTA
                                               A1-ICUENTA
           ELSE
              MOVE ZM608-CCTAINVPAT         TO R1-01-DET-CCTAINVPAT
                                               A1-CCTAINVPAT
           END-IF.
      *
      * FSW-1.0.0-I
      ***  MOVE NOMBRE   OF DCLCUENTA       TO WS-NOMBRE
      ***  MOVE NAPELL1  OF DCLCUENTA       TO WS-PATERNO
      ***  MOVE NAPELL2  OF DCLCUENTA       TO WS-MATERNO
      ***  MOVE WS-NOMBRE-CTE               TO WS-STRING
      ***  MOVE 90                          TO WS-LONG
           PERFORM 6003-OBTIENE-PERJUR
           IF SPERJUR OF DCLPERJUR = 'F'
           OR IEMPR   OF DCLCUENTA = 'BCM'
      *--- PERSONAS FIS. Y BANCO SE AGRUPA POR: NOMBRE, NAPELL1, NAPELL2
              MOVE SPACES                TO W000-CONCATENA
              MOVE NOMBRE  OF DCLCUENTA  TO W000-CAMPO1
              MOVE NAPELL1 OF DCLCUENTA  TO W000-CAMPO2
              MOVE NAPELL2 OF DCLCUENTA  TO W000-CAMPO3
              MOVE 90                    TO WS-LONG
              MOVE W000-CONCATENA        TO WS-STRING
           ELSE
      *--- PERSONAS MORALES SE AGRUPA POR: NAPELL1, NAPELL2, NOMBRE
              MOVE SPACES                TO W000-CONCATENA-MOR
              MOVE NAPELL1 OF DCLCUENTA  TO W000-CAMPO1-MOR
              MOVE NAPELL2 OF DCLCUENTA  TO W000-CAMPO2-MOR
              MOVE NOMBRE  OF DCLCUENTA  TO W000-CAMPO3-MOR
              MOVE 90                    TO WS-LONG
              MOVE W000-CONCATENA-MOR    TO WS-STRING
           END-IF
      * FSW-1.0.0-F
      *
           CALL RUT-ZM5RG003             USING WS-LONG WS-STRING
           MOVE WS-STRING                   TO R1-01-DET-NOMBRE
                                               A1-NOMBRE
      *
           IF IEMPR      OF DCLCUENTA = 'BCM' AND
              ISUCCASA   OF DCLCUENTA NOT = 'CTD'
              MOVE DCALLE  OF DCLCUENTA     TO WS-DIRECC-PARTE1
              MOVE DCALLEP OF DCLCUENTA     TO WS-DIRECC-PARTE2
              MOVE DESTADO OF DCLCUENTA     TO WS-NUM-EXT-INT
              MOVE WS-DIRECCION             TO WS-STRING
              MOVE 90                       TO WS-LONG
              CALL RUT-ZM5RG003          USING WS-LONG WS-STRING
              MOVE WS-STRING                TO R1-02-DET-DIRECCION
                                               A1-DIRECCION
      *
              MOVE DCOLON   OF DCLCUENTA    TO WS-COLON-PARTE1
              MOVE DCOLONP  OF DCLCUENTA    TO WS-COLON-PARTE2
              MOVE WS-COLONIA               TO WS-STRING
              MOVE 90                       TO WS-LONG
              CALL RUT-ZM5RG003          USING WS-LONG WS-STRING
              MOVE WS-STRING                TO R1-03-DET-COLONIA
                                               A1-COLONIA
      *
              MOVE DPOBLA   OF DCLCUENTA    TO WS-CIUDAD
              MOVE WS-SEPARADOR             TO WS-SEP
              MOVE DPOBLAP  OF DCLCUENTA    TO WS-ESTADO
              MOVE WS-CIUDAD-ESTADO         TO WS-STRING
              MOVE 90                       TO WS-LONG
              CALL RUT-ZM5RG003          USING WS-LONG WS-STRING
              MOVE WS-STRING                TO R1-03-DET-POBLACION
                                               A1-POBLACION
              MOVE IPOS     OF DCLCUENTA    TO R1-03-DET-IPOS
                                               A1-IPOS
           ELSE
              IF DCALLEP OF DCLCUENTA = ' '
                 MOVE DCALLE   OF DCLCUENTA TO R1-02-DET-DIRECCION
                                               A1-DIRECCION
                 MOVE DCOLON   OF DCLCUENTA TO R1-03-DET-COLONIA
                                               A1-COLONIA
                 MOVE DPOBLA   OF DCLCUENTA TO WS-CIUDAD
                 MOVE WS-SEPARADOR          TO WS-SEP
                 MOVE DESTADO  OF DCLCUENTA TO WS-ESTADO
                 MOVE WS-CIUDAD-ESTADO      TO WS-STRING
                 MOVE 90                    TO WS-LONG
                 CALL RUT-ZM5RG003       USING WS-LONG WS-STRING
                 MOVE WS-STRING             TO R1-03-DET-POBLACION
                                               A1-POBLACION
                 MOVE IPOS     OF DCLCUENTA TO R1-03-DET-IPOS
                                               A1-IPOS
              ELSE
                 MOVE DCALLEP  OF DCLCUENTA TO R1-02-DET-DIRECCION
                                               A1-DIRECCION
                 MOVE DCOLONP  OF DCLCUENTA TO R1-03-DET-COLONIA
                                               A1-COLONIA
                 MOVE DPOBLAP  OF DCLCUENTA TO WS-CIUDAD
                 MOVE WS-SEPARADOR          TO WS-SEP
                 MOVE DESTADOP OF DCLCUENTA TO WS-ESTADO
                 MOVE WS-CIUDAD-ESTADO      TO WS-STRING
                 MOVE 90                    TO WS-LONG
                 CALL RUT-ZM5RG003       USING WS-LONG WS-STRING
                 MOVE WS-STRING             TO R1-03-DET-POBLACION
                                               A1-POBLACION
                 MOVE IPOSP    OF DCLCUENTA TO R1-03-DET-IPOS
                                               A1-IPOS
              END-IF
           END-IF.
      *
           MOVE IRFC     OF DCLCUENTA       TO WS-IRFC
      *
           IF WS-IRFC (01:03)    IS ALPHABETIC AND
              WS-IRFC (04:06)    IS NUMERIC    AND
              WS-IRFC (10:01)     = SPACES     AND
              WS-IRFC (11:03) NOT = SPACES
              MOVE WS-IRFC (11:03)          TO WS-IRFC (10:03)
              MOVE SPACES                   TO WS-IRFC (13:01)
           END-IF.
      *
           IF WS-RFC-ALFA-4 IS NOT ALPHABETIC AND
              WS-RFC-ALFA-4 NOT = ' '         AND
              WS-RFC-ALFA-4 NOT = '-'
              MOVE WS-IRFC                  TO WS-IRFC1-OK
              MOVE WS-IRFC1                 TO WS-IRFC
           ELSE
              IF WS-RFC-ALFA-4  = '-'
                 MOVE ' '                   TO WS-RFC-ALFA-4
              END-IF
           END-IF.
      *
           MOVE WS-RFC-ALFA                 TO WS-F01-RFC-ALFA
           MOVE WS-RFC-NUM                  TO WS-F01-RFC-NUM
           IF WS-RFC-HOMO = SPACES
              MOVE SPACES                   TO WS-F01-RFC-HOMO
           ELSE
              MOVE WS-RFC-HOMO              TO WS-F01-RFC-HOMO
           END-IF.
      *
           MOVE WS-F01-RFC-ALFA             TO WS-F01-RFC-ALFA1
           MOVE WS-F01-RFC-NUM              TO WS-F01-RFC-NUM1
           MOVE WS-F01-RFC-HOMO             TO WS-F01-RFC-HOMO1
      *
           MOVE WS-F01-RFC-FBCM             TO WS-STRING-ENTRADA
           MOVE 13                          TO WS-LONG-STRING
           PERFORM 9886-ALINEA-STRING-IZQ
           MOVE WS-STRING-SALIDA            TO WS-F01-RFC-FBCM
      *
           MOVE WS-F01-RFC-FBCM             TO R1-02-DET-IRFC
                                               A1-IRFC
      *
           MOVE WS-TOT-VTA-CTA              TO R1-01-DET-TOT-VTAS
                                               A1-TOT-VTAS
           MOVE WS-TOT-TIT-CTA              TO R1-01-DET-TIT-DONAC
                                               A1-TIT-DONAC
           MOVE WS-TOT-MNT-CTA              TO R1-01-DET-MNT-DONAC
                                               A1-MNT-DONAC
      *
           ADD  1                           TO WS-TOT-REG-SER
                                               WS-TOT-REG-EMI
                                               WS-TOT-REG-EMP
           ADD  WS-TOT-VTA-CTA              TO WS-TOT-VTA-SER
                                               WS-TOT-VTA-EMI
                                               WS-TOT-VTA-EMP
           ADD  WS-TOT-TIT-CTA              TO WS-TOT-TIT-SER
           ADD  WS-TOT-MNT-CTA              TO WS-TOT-MNT-SER
                                               WS-TOT-MNT-EMI
                                               WS-TOT-MNT-EMP
      *
           IF WS-ARC-ENCAB = ZEROS
              WRITE REG-SECUENC-TXT       FROM A1-ENCABEZADO
              ADD 1                         TO WS-ARC-ENCAB
           END-IF
           WRITE REG-SECUENC-TXT          FROM A1-DETALLE
      *
           IF R1-NUM-LIN + 5 > R1-MAX-LIN
              PERFORM 160-ENCABEZADO
           END-IF
           IF WS-EMP = 'CBP'
              WRITE REG-LISTADO-CBP       FROM R1-01-DETA
              WRITE REG-LISTADO-CBP       FROM R1-02-DETA
              WRITE REG-LISTADO-CBP       FROM R1-03-DETA
           ELSE
              WRITE REG-LISTADO-BCM       FROM R1-01-DETA
              WRITE REG-LISTADO-BCM       FROM R1-02-DETA
              WRITE REG-LISTADO-BCM       FROM R1-03-DETA
           END-IF
           ADD 3                            TO R1-NUM-LIN.
       020-FIN.
           EXIT.
      *
      * FSW-1.0.0-I
      ******************************************************************
      *   CIERRA EL CURSOR DE LA TABLA OPERASI VENTAS A PRORRATEAR
      ******************************************************************
       6003-OBTIENE-PERJUR.
            MOVE IPERJUR   OF DCLCUENTA  TO IPERJUR  OF DCLPERJUR
            MOVE ISUBCLAS  OF DCLCUENTA  TO ISUBCLAS OF DCLPERJUR
            MOVE 'M'                     TO SPERJUR  OF DCLPERJUR
            EXEC SQL
                 SELECT SPERJUR
                   INTO :DCLPERJUR.SPERJUR
                   FROM PERJUR
                  WHERE IPERJUR  = :DCLPERJUR.IPERJUR
                    AND ISUBCLAS = :DCLPERJUR.ISUBCLAS
            END-EXEC.
       6003-FIN.
           EXIT.
      * FSW-1.0.0-F
      *
      *
CIHM  ******************************************************************
      *  EXTRAE EL CORREO ELECTRONICO DE LOS CLIENTES PARA INTEGRARLOS
      *  AL ARCHIVO FUNDACION_MUV
      ******************************************************************
      *
       6050-OBTIENE-EMAIL.
            EXEC SQL
                SELECT  VALUE(B.NEMAIL,' ')
                  INTO  :DCLCTECOMP.NEMAIL
                  FROM  ACLICTA A, CTECOMP B
                 WHERE  A.ICUENTA  = :DCLCUENTA.ICUENTA
                   AND  A.ICLIENTE = B.ICLIENTE
                 FETCH  FIRST ROW ONLY
            END-EXEC.
            IF SQLCODE NOT = ZEROS
                  MOVE SPACES TO NEMAIL OF DCLCTECOMP
            END-IF.
      *
       6050-FIN.
CIHM       EXIT.
      ******************************************************************
      *   REALIZA CARGO A LA CUENTA CONCENTRADORA Y ABONO A LA CUENTA
      *   DE CHEQUES DE LA FUNDACION BANCOMER
      ******************************************************************
       030-REALIZA-CARGO-ABONO.
      *
      *--- INSERTA MOVIMIENTO DE CARGO DE EFECTIVO A LA CUENTA
      *--- CONCENTRADORA
      *
      *----
           INITIALIZE                      ZG11-REGISTRO
           MOVE '11'                    TO ZG11-ITIPOFOL
           MOVE SPACES                  TO ZG11-ISUCCASA
      *----
           CALL RUT-ZM6RG011         USING ZG11-REGISTRO
      *----
           IF ZG11-WCODRET = ZEROS
              MOVE ZG11-SIGFOLIO        TO W000-SIGFOLIO
           ELSE
              MOVE ZG11-SIGFOLIO        TO W000-SIGFOLIO
              DISPLAY W000-PROG '  ERROR EN RUTINA DE FOLIOS '
                                                        RUT-ZM6RG011
              DISPLAY W000-PROG '     ZG11-MENSERR  : ' ZG11-MENSERR
              DISPLAY W000-PROG '     ZG11-WCODRET  : ' ZG11-WCODRET
              DISPLAY W000-PROG '     ZG11-SQLCODE  : ' ZG11-SQLCODE
              DISPLAY W000-PROG '     ZG11-SIGFOLIO : ' ZG11-SIGFOLIO
              PERFORM 999-ABORTA
           END-IF.
      *-----
      *
           INITIALIZE                      DCLZMDT633
           MOVE W000-SIGFOLIO           TO ZM633-IREF
           MOVE 'TES'                   TO ZM633-TPOMERC
           MOVE W000-SIGFOLIO           TO ZM633-NUFOLIO
           MOVE 'VTA DONATIVO'          TO ZM633-NUCVEISI
           MOVE ZEROS                   TO ZM633-NUPAPEL
           MOVE 'A'                     TO ZM633-SESTATUS
           MOVE SPACES                  TO ZM633-TPOPERA
           MOVE 1120                    TO ZM633-IOPERA
           MOVE ICUENTA  OF DCLCUENTA   TO ZM633-ICUENTA
           MOVE WS-CUENTA-CARGO         TO ZM633-CONTRATO
           MOVE ZEROS                   TO ZM633-NUMMOV
           MOVE 'C'                     TO ZM633-TPOMOV
           MOVE W000-FECHA-ACTUAL       TO ZM633-FHFOLIO
           MOVE W000-FECHA-ACTUAL       TO ZM633-FHAPLICA
           MOVE W000-FECHA-ACTUAL       TO ZM633-FHVENCIM
           MOVE WS-TOT-TIT-CTA          TO ZM633-CTITULOS
           MOVE ZEROS                   TO ZM633-PCDCTO
           MOVE ZEROS                   TO ZM633-CTPRECIO
           MOVE ZEROS                   TO ZM633-PCCORRO
           MOVE WS-TOT-MNT-CTA          TO ZM633-MAFECTA
           MOVE ZEROS                   TO ZM633-NUCVEOPE
           IF ISUCCASA OF DCLCUENTA = '032'
              MOVE 900                  TO ZM633-NUPLAZA
           ELSE
              MOVE ZEROS                TO ZM633-NUPLAZA
           END-IF
           MOVE 1                       TO ZM633-TPMONEDA
           MOVE ZEROS                   TO ZM633-NUCUENTA
           MOVE SPACES                  TO ZM633-CDORIGEN
           MOVE W000-PROG               TO ZM633-IPROGRAM
           MOVE ISERIE   OF DCLCONCEPT  TO ZM633-ISERIE
           MOVE SPACES                  TO ZM633-ITVBMV
           MOVE IEMISORA OF DCLCONCEPT  TO ZM633-NBINSTRU
           MOVE ZEROS                   TO ZM633-NFOLIO
           MOVE ZEROS                   TO ZM633-ISEC
           MOVE SPACES                  TO ZM633-NPROCESO
           MOVE SPACES                  TO ZM633-NUSUARIO
           EXEC SQL
              INSERT INTO ZMDT633
                     (ZM633_IREF,
                      ZM633_TPOMERC,
                      ZM633_NUFOLIO,
                      ZM633_NUCVEISI,
                      ZM633_NUPAPEL,
                      ZM633_SESTATUS,
                      ZM633_TPOPERA,
                      ZM633_IOPERA,
                      ZM633_ICUENTA,
                      ZM633_CONTRATO,
                      ZM633_NUMMOV,
                      ZM633_TPOMOV,
                      ZM633_FHFOLIO,
                      ZM633_FHAPLICA,
                      ZM633_FHVENCIM,
                      ZM633_CTITULOS,
                      ZM633_PCDCTO,
                      ZM633_CTPRECIO,
                      ZM633_PCCORRO,
                      ZM633_MAFECTA,
                      ZM633_NUCVEOPE,
                      ZM633_NUPLAZA,
                      ZM633_TPMONEDA,
                      ZM633_NUCUENTA,
                      ZM633_CDORIGEN,
                      ZM633_IPROGRAM,
                      ZM633_FREG,
                      ZM633_ISERIE,
                      ZM633_ITVBMV,
                      ZM633_NBINSTRU,
                      ZM633_NFOLIO,
                      ZM633_ISEC,
                      ZM633_NPROCESO,
                      ZM633_NUSUARIO)
             VALUES (:ZM633-IREF,
                     :ZM633-TPOMERC,
                     :ZM633-NUFOLIO,
                     :ZM633-NUCVEISI,
                     :ZM633-NUPAPEL,
                     :ZM633-SESTATUS,
                     :ZM633-TPOPERA,
                     :ZM633-IOPERA,
                     :ZM633-ICUENTA,
                     :ZM633-CONTRATO,
                     :ZM633-NUMMOV,
                     :ZM633-TPOMOV,
                     :ZM633-FHFOLIO,
                     :ZM633-FHAPLICA,
                     :ZM633-FHVENCIM,
                     :ZM633-CTITULOS,
                     :ZM633-PCDCTO,
                     :ZM633-CTPRECIO,
                     :ZM633-PCCORRO,
                     :ZM633-MAFECTA,
                     :ZM633-NUCVEOPE,
                     :ZM633-NUPLAZA,
                     :ZM633-TPMONEDA,
                     :ZM633-NUCUENTA,
                     :ZM633-CDORIGEN,
                     :ZM633-IPROGRAM,
                      CURRENT TIMESTAMP,
                     :ZM633-ISERIE,
                     :ZM633-ITVBMV,
                     :ZM633-NBINSTRU,
                     :ZM633-NFOLIO,
                     :ZM633-ISEC,
                     :ZM633-NPROCESO,
                     :ZM633-NUSUARIO)
           END-EXEC
           IF SQLCODE NOT = ZEROS
              DISPLAY W000-PROG ' NO SE REALIZO ALTA EN ZMDT633'
              DISPLAY W000-PROG ' 030-REALIZA-CARGO-ABONO.     '
              DISPLAY W000-PROG 'ZM633_IREF     (' ZM633-IREF  ')'
              DISPLAY W000-PROG 'ZM633_ICUENTA  (' ZM633-ICUENTA ')'
              DISPLAY W000-PROG 'ZM633_CONTRATO (' ZM633-CONTRATO ')'
              DISPLAY W000-PROG 'ZM633_MAFECTA  (' ZM633-MAFECTA ')'
              MOVE SQLCODE              TO WS-SQLCODE
              DISPLAY 'SQLCODE ' WS-SQLCODE
              DISPLAY 'SE TERMINA EL PROGRAMA                '
              PERFORM 999-ABORTA
           END-IF.
      *
      *
      *--- INSERTA MOVIMIENTO DE ABONO DE EFECTIVO A LA CUENTA
      *--- DE CHEQUES DE LA FUNDACION BANCOMER
      *
      *----
           INITIALIZE                      ZG11-REGISTRO
           MOVE '11'                    TO ZG11-ITIPOFOL
           MOVE SPACES                  TO ZG11-ISUCCASA
      *----
           CALL RUT-ZM6RG011         USING ZG11-REGISTRO
      *----
           IF ZG11-WCODRET = ZEROS
              MOVE ZG11-SIGFOLIO        TO W000-SIGFOLIO
           ELSE
              MOVE ZG11-SIGFOLIO        TO W000-SIGFOLIO
              DISPLAY W000-PROG '  ERROR EN RUTINA DE FOLIOS '
                                                        RUT-ZM6RG011
              DISPLAY W000-PROG '     ZG11-MENSERR  : ' ZG11-MENSERR
              DISPLAY W000-PROG '     ZG11-WCODRET  : ' ZG11-WCODRET
              DISPLAY W000-PROG '     ZG11-SQLCODE  : ' ZG11-SQLCODE
              DISPLAY W000-PROG '     ZG11-SIGFOLIO : ' ZG11-SIGFOLIO
              PERFORM 999-ABORTA
           END-IF.
      *-----
      *
           INITIALIZE                      DCLZMDT633
           MOVE W000-SIGFOLIO           TO ZM633-IREF
           MOVE 'TES'                   TO ZM633-TPOMERC
           MOVE W000-SIGFOLIO           TO ZM633-NUFOLIO
           MOVE 'VTA DONATIVO'          TO ZM633-NUCVEISI
           MOVE ZEROS                   TO ZM633-NUPAPEL
           MOVE 'A'                     TO ZM633-SESTATUS
           MOVE SPACES                  TO ZM633-TPOPERA
           MOVE 1020                    TO ZM633-IOPERA
           MOVE ICUENTA  OF DCLCUENTA   TO ZM633-ICUENTA
           MOVE WS-CUENTA-ABONO         TO ZM633-CONTRATO
           MOVE ZEROS                   TO ZM633-NUMMOV
           MOVE 'A'                     TO ZM633-TPOMOV
           MOVE W000-FECHA-ACTUAL       TO ZM633-FHFOLIO
           MOVE W000-FECHA-ACTUAL       TO ZM633-FHAPLICA
           MOVE W000-FECHA-ACTUAL       TO ZM633-FHVENCIM
           MOVE WS-TOT-TIT-CTA          TO ZM633-CTITULOS
           MOVE ZEROS                   TO ZM633-PCDCTO
           MOVE ZEROS                   TO ZM633-CTPRECIO
           MOVE ZEROS                   TO ZM633-PCCORRO
           MOVE WS-TOT-MNT-CTA          TO ZM633-MAFECTA
           MOVE ZEROS                   TO ZM633-NUCVEOPE
           IF ISUCCASA OF DCLCUENTA = '032'
              MOVE 900                  TO ZM633-NUPLAZA
           ELSE
              MOVE ZEROS                TO ZM633-NUPLAZA
           END-IF
           MOVE 1                       TO ZM633-TPMONEDA
           MOVE ZEROS                   TO ZM633-NUCUENTA
           MOVE SPACES                  TO ZM633-CDORIGEN
           MOVE W000-PROG               TO ZM633-IPROGRAM
           MOVE ISERIE   OF DCLCONCEPT  TO ZM633-ISERIE
           MOVE SPACES                  TO ZM633-ITVBMV
           MOVE IEMISORA OF DCLCONCEPT  TO ZM633-NBINSTRU
           MOVE ZEROS                   TO ZM633-NFOLIO
           MOVE ZEROS                   TO ZM633-ISEC
           MOVE SPACES                  TO ZM633-NPROCESO
           MOVE SPACES                  TO ZM633-NUSUARIO
           EXEC SQL
              INSERT INTO ZMDT633
                     (ZM633_IREF,
                      ZM633_TPOMERC,
                      ZM633_NUFOLIO,
                      ZM633_NUCVEISI,
                      ZM633_NUPAPEL,
                      ZM633_SESTATUS,
                      ZM633_TPOPERA,
                      ZM633_IOPERA,
                      ZM633_ICUENTA,
                      ZM633_CONTRATO,
                      ZM633_NUMMOV,
                      ZM633_TPOMOV,
                      ZM633_FHFOLIO,
                      ZM633_FHAPLICA,
                      ZM633_FHVENCIM,
                      ZM633_CTITULOS,
                      ZM633_PCDCTO,
                      ZM633_CTPRECIO,
                      ZM633_PCCORRO,
                      ZM633_MAFECTA,
                      ZM633_NUCVEOPE,
                      ZM633_NUPLAZA,
                      ZM633_TPMONEDA,
                      ZM633_NUCUENTA,
                      ZM633_CDORIGEN,
                      ZM633_IPROGRAM,
                      ZM633_FREG,
                      ZM633_ISERIE,
                      ZM633_ITVBMV,
                      ZM633_NBINSTRU,
                      ZM633_NFOLIO,
                      ZM633_ISEC,
                      ZM633_NPROCESO,
                      ZM633_NUSUARIO)
             VALUES (:ZM633-IREF,
                     :ZM633-TPOMERC,
                     :ZM633-NUFOLIO,
                     :ZM633-NUCVEISI,
                     :ZM633-NUPAPEL,
                     :ZM633-SESTATUS,
                     :ZM633-TPOPERA,
                     :ZM633-IOPERA,
                     :ZM633-ICUENTA,
                     :ZM633-CONTRATO,
                     :ZM633-NUMMOV,
                     :ZM633-TPOMOV,
                     :ZM633-FHFOLIO,
                     :ZM633-FHAPLICA,
                     :ZM633-FHVENCIM,
                     :ZM633-CTITULOS,
                     :ZM633-PCDCTO,
                     :ZM633-CTPRECIO,
                     :ZM633-PCCORRO,
                     :ZM633-MAFECTA,
                     :ZM633-NUCVEOPE,
                     :ZM633-NUPLAZA,
                     :ZM633-TPMONEDA,
                     :ZM633-NUCUENTA,
                     :ZM633-CDORIGEN,
                     :ZM633-IPROGRAM,
                      CURRENT TIMESTAMP,
                     :ZM633-ISERIE,
                     :ZM633-ITVBMV,
                     :ZM633-NBINSTRU,
                     :ZM633-NFOLIO,
                     :ZM633-ISEC,
                     :ZM633-NPROCESO,
                     :ZM633-NUSUARIO)
           END-EXEC
           IF SQLCODE NOT = ZEROS
              DISPLAY W000-PROG ' NO SE REALIZO ALTA EN ZMDT633'
              DISPLAY W000-PROG ' 030-REALIZA-CARGO-ABONO.     '
              DISPLAY W000-PROG 'ZM633_IREF     (' ZM633-IREF  ')'
              DISPLAY W000-PROG 'ZM633_ICUENTA  (' ZM633-ICUENTA ')'
              DISPLAY W000-PROG 'ZM633_CONTRATO (' ZM633-CONTRATO ')'
              DISPLAY W000-PROG 'ZM633_MAFECTA  (' ZM633-MAFECTA ')'
              MOVE SQLCODE              TO WS-SQLCODE
              DISPLAY 'SQLCODE ' WS-SQLCODE
              DISPLAY 'SE TERMINA EL PROGRAMA                '
              PERFORM 999-ABORTA
           END-IF.
      *
      *    INSERTA REGISTRO EN ARCHIVO INTERFAZ FUNDACION BANCOMER
      *
           INITIALIZE                               A3-DETALLE.
           MOVE '1'                              TO A3-TIPO-REG
           IF WS-EMP = 'BCM'
              MOVE 'MUV '                        TO A3-APLICACION
              MOVE DCALLE  OF DCLCUENTA          TO WS-DIRECC-PARTE1
              MOVE DCALLEP OF DCLCUENTA          TO WS-DIRECC-PARTE2
              MOVE SPACES                        TO WS-NUM-EXT-INT
              MOVE WS-DIRECCION                  TO WS-STRING
              MOVE 90                            TO WS-LONG
              CALL RUT-ZM5RG003               USING WS-LONG WS-STRING
              MOVE WS-STRING                     TO A3-DIRECCION
              MOVE DESTADO  OF DCLCUENTA (01:09) TO A3-NUMERO-EXTERIOR
              MOVE DESTADO  OF DCLCUENTA (10:09) TO A3-NUMERO-INTERIOR
              MOVE DESTADOP OF DCLCUENTA         TO A3-PAIS
           ELSE
              MOVE 'CBP '                        TO A3-APLICACION
              MOVE A1-DIRECCION                  TO A3-DIRECCION
              MOVE SPACES                        TO A3-NUMERO-EXTERIOR
              MOVE SPACES                        TO A3-NUMERO-INTERIOR
              MOVE SPACES                        TO A3-PAIS
           END-IF
           MOVE ZEROS                       TO A3-FILLER1-CEROS.
           MOVE A1-CCTAINVPAT               TO A3-CCTAINVPAT.
           MOVE ZEROS                       TO A3-FILLER2-CEROS.
           MOVE W000-SIGFOLIO               TO A3-NUM-OPERAC-IREF.
      * SDAT-39213I
           INITIALIZE WS-A3-NOMBRE-AUX.
           MOVE A1-NOMBRE                   TO WS-A3-NOMBRE-AUX.
           PERFORM 035-LIMPIA-NOMBRE.
           MOVE WS-A3-NOMBRE-AUX            TO A3-NOMBRE.
      * SDAT-39213F
           MOVE A1-IRFC                     TO A3-IRFC.
           MOVE WS-TOT-MNT-CTA              TO A3-MNT-DONAC.
           MOVE W000-FECHA-FIN-MES          TO A3-FECHA-FIN-MES.
           ACCEPT WS-HORA-SISTEMA         FROM TIME.
           MOVE '00:00:00'                  TO W000-HORA-FIN-MES.
           MOVE WS-HH                       TO W000-HH-FIN-MES.
           MOVE WS-MM                       TO W000-MM-FIN-MES.
           MOVE WS-SS                       TO W000-SS-FIN-MES.
           MOVE W000-HORA-FIN-MES           TO A3-HORA-FIN-MES.
           MOVE A1-COLONIA                  TO A3-COLONIA.
           MOVE WS-CIUDAD                   TO A3-POBLACION.
           MOVE WS-ESTADO                   TO A3-ESTADO.
           MOVE A1-IPOS                     TO A3-COD-POSTAL.
           MOVE SPACES                      TO A3-ZONA-DE-REPARTO.
      * SDAT-39213I
           PERFORM 032-OBTIENE-REGIMEN-FISCAL.
           MOVE WSV-REGI-FISREC-PARAM       TO A3-REG-FISCAL.
           INITIALIZE WS-BANDERA-ENCONTRO.
      * SDAT-39213F
      *
      *INICIA
           MOVE ICUENTA  OF DCLCUENTA       TO ZM609-NICUENTA
           MOVE SPACES                      TO ZM609-CTAABPIGO
           EXEC SQL
              SELECT  ZM609_CTAABPIGO
                INTO :ZM609-CTAABPIGO
                FROM ZMDT609
               WHERE ZM609_NICUENTA = :ZM609-NICUENTA
           END-EXEC
           IF SQLCODE = ZEROS AND
              ZM609-CTAABPIGO IS NUMERIC
              MOVE ZM609-CTAABPIGO          TO WS-CCTAABPIGO
              MOVE WS-CCTAABPIGO-NCTA       TO WS-CUENTA-ECT
      * SDAT-39213I
              PERFORM 031-OBTIENE-NOMBRE-FISCAL
      * SDAT-39213F
           ELSE
              MOVE ICUENTA  OF DCLCUENTA    TO ZM608-NICUENTA
              MOVE SPACES                   TO ZM608-CCTAINVPAT
              EXEC SQL
                 SELECT  ZM608_CCTAINVPAT
                   INTO :ZM608-CCTAINVPAT
                   FROM ZMDT608
                  WHERE ZM608_NICUENTA = :ZM608-NICUENTA
              END-EXEC
              IF SQLCODE = ZEROS AND
                 ZM608-CCTAINVPAT IS NUMERIC
                 MOVE ZM608-CCTAINVPAT      TO WS-CCTAINVPAT
                 MOVE WS-CCTAINVPAT-NCTA    TO WS-CUENTA-ECT
              ELSE
                 MOVE ICUENTA  OF DCLCUENTA TO WS-CUENTA-ECT
              END-IF
           END-IF.
      * SDAT-39213I
           PERFORM 033-OBTIENE-CP-FISCAL.
      * SDAT-39213F
           MOVE WS-CUENTA-ECT-4-ULT         TO A3-NUM-CTA-ULT-4-DIG.
           MOVE '02'                        TO A3-METODO-DE-PAGO.
      *TERMINA
      *
           MOVE SPACES                      TO A3-FILLER.
      *
CIHM       PERFORM 6050-OBTIENE-EMAIL
CIHM          MOVE NEMAIL TO A3-EMAIL
      *
           WRITE REG-INTEF-FUNDA          FROM A3-DETALLE.
       030-FIN.
           EXIT.
      * SDAT-39213I
      *
      ******************************************************************
      *   OBTIENE NOMBRE FISCAL
      ******************************************************************
       031-OBTIENE-NOMBRE-FISCAL.
           MOVE WS-CCTAABPIGO-NCTA   TO ZM895-CTA-CFDI.
           MOVE WSC-COT-TIT          TO ZM895-COT-TIT.
           MOVE WS-EMP               TO ZM895-IEMPR.
           MOVE WSC-CODSER           TO ZM895-CODISER.
           MOVE WSC-N                TO WS-BANDERA-ENCONTRO.
           IF IEMPR   OF DCLCUENTA = 'BCM'
              EXEC SQL
                 SELECT ZM895_NOMBRE,
                        ZM895_APELL1,
                        ZM895_APELL2,
                        ZM895_CPFISCAL
                 INTO  :ZM895-NOMBRE,
                       :ZM895-APELL1,
                       :ZM895-APELL2,
                       :ZM895-CPFISCAL
                 FROM ZMDT895
                 WHERE ZM895_CTA_CFDI = :ZM895-CTA-CFDI
                 AND   ZM895_COT_TIT  = :ZM895-COT-TIT
                 AND   ZM895_IEMPR    = :ZM895-IEMPR
              END-EXEC
           ELSE
              EXEC SQL
                 SELECT ZM895_NOMBRE,
                        ZM895_APELL1,
                        ZM895_APELL2,
                        ZM895_CPFISCAL
                 INTO  :ZM895-NOMBRE,
                       :ZM895-APELL1,
                       :ZM895-APELL2,
                       :ZM895-CPFISCAL
                 FROM ZMDT895
                 WHERE ZM895_CTA_CFDI = :ZM895-CTA-CFDI
                 AND   ZM895_COT_TIT  = :ZM895-COT-TIT
                 AND   ZM895_IEMPR    = :ZM895-IEMPR
                 AND   ZM895_CODISER  = :ZM895-CODISER
              END-EXEC
           END-IF
           EVALUATE SQLCODE
              WHEN ZEROS
                 MOVE  'S'  TO WS-BANDERA-ENCONTRO
                 IF ZM895-NOMBRE OF DCLZMDT895 EQUAL SPACES
                    MOVE SPACES                       TO
                                                      W000-CONCAT-NF
                    MOVE ZM895-APELL1 OF DCLZMDT895   TO W000-NF-APE1
                    MOVE ZM895-APELL2 OF DCLZMDT895   TO W000-NF-APE2
                    MOVE ZM895-NOMBRE OF DCLZMDT895   TO W000-NF-NOMBRE
                    MOVE 285                          TO WS-LONG-NF
                    MOVE W000-CONCAT-NF               TO WS-STRING-NF
                 ELSE
                    MOVE SPACES                       TO W000-CONCAT-NF
                    MOVE ZM895-NOMBRE OF DCLZMDT895   TO W000-NF-NOMBRE
                    MOVE ZM895-APELL1 OF DCLZMDT895   TO W000-NF-APE1
                    MOVE ZM895-APELL2 OF DCLZMDT895   TO W000-NF-APE2
                    MOVE 285                          TO WS-LONG-NF
                    MOVE W000-CONCAT-NF               TO WS-STRING-NF
                 END-IF
      *
                 INITIALIZE WS-A3-NOMBRE-AUX
                 CALL RUT-ZM5RG003         USING WS-LONG-NF WS-STRING-NF
                 MOVE WS-STRING-NF             TO WS-A3-NOMBRE-AUX
                 PERFORM 035-LIMPIA-NOMBRE
                 MOVE WS-A3-NOMBRE-AUX         TO A3-NOMBRE
              WHEN 100
                 CONTINUE
              WHEN OTHER
                 DISPLAY 'ERROR AL LEER  ZMDT895 '
                 DISPLAY 'ZM895-CTA-CFDI ' ZM895-CTA-CFDI
                 DISPLAY 'ZM895-COT-TIT  ' ZM895-COT-TIT
                 MOVE SQLCODE          TO WS-SQLCODE
                 DISPLAY 'SQLCODE ' WS-SQLCODE
                 DISPLAY 'SE TERMINA EL PROGRAMA                '
                 PERFORM 999-ABORTA
           END-EVALUATE.
       031-FIN.
           EXIT.
      *
      ******************************************************************
      *   OBTIENE REGIMEN FISCAL
      ******************************************************************
       032-OBTIENE-REGIMEN-FISCAL.
           INITIALIZE Z005-REGS

           MOVE 'S'                TO Z005-OPCION
           MOVE 'RF4'              TO Z005-ITIPOPAR
           MOVE IPERJUR OF DCLCUENTA TO WSV-IPERJUR-PARAM
           MOVE ISUBCLAS OF DCLCUENTA TO WSV-ISUBCLAS-PARAM
           MOVE WSV-IPARAM         TO Z005-IPARAM
           MOVE SPACES             TO Z005-DATOSPAR

           CALL RUT-ZM6LR005 USING Z005-REGS

              IF Z005-A-WCODRET EQUAL ZEROS
                 MOVE Z005-DATOSPAR         TO WSV-DATOSPAR
              ELSE
                 IF SPERJUR OF DCLPERJUR = 'F'
                    MOVE '605D04'           TO WSV-DATOSPAR
                 ELSE
                    MOVE '601G03'           TO WSV-DATOSPAR
                 END-IF
              END-IF.
       032-FIN.
           EXIT.
      *
      ******************************************************************
      *   OBTIENE CODIGO POSTAL FISCAL
      ******************************************************************
       033-OBTIENE-CP-FISCAL.
           IF WS-EMP = 'BCM'
      * BANCA PATRIMONIAL
              IF WS-BANDERA-ENCONTRO = 'S'
                 MOVE ZM895-CPFISCAL OF DCLZMDT895 TO A3-CP-FISCAL
              ELSE
                 MOVE A3-COD-POSTAL                TO A3-CP-FISCAL
              END-IF
           ELSE
      * CASA DE BOLSA
              PERFORM 034-BUSCA-CTE-ACLICTA
              MOVE ICUENTA OF DCLCUENTA TO ZMDIR-ICUENTA  OF DCLZMDTDIR
              MOVE WSC-TIPCTA           TO ZMDIR-ITIPOCTA OF DCLZMDTDIR
              MOVE WSC-TIPDIR           TO ZMDIR-ITIPODIR OF DCLZMDTDIR
              EXEC SQL
                 SELECT ZMDIR_IPOS
                 INTO  :ZMDIR-IPOS
                 FROM ZMDTDIR
                 WHERE ZMDIR_ICUENTA  = :DCLZMDTDIR.ZMDIR-ICUENTA
                   AND ZMDIR_ICLIENTE = :DCLZMDTDIR.ZMDIR-ICLIENTE
                   AND ZMDIR_ITIPOCTA = :DCLZMDTDIR.ZMDIR-ITIPOCTA
                   AND ZMDIR_ITIPODIR = :DCLZMDTDIR.ZMDIR-ITIPODIR
              END-EXEC
              EVALUATE SQLCODE
                 WHEN ZEROS
                    IF ZMDIR-IPOS OF DCLZMDTDIR NOT EQUAL ZEROES
                       MOVE ZMDIR-IPOS OF DCLZMDTDIR TO A3-CP-FISCAL
                    ELSE
                       MOVE A3-COD-POSTAL            TO A3-CP-FISCAL
                    END-IF
                 WHEN 100
                       MOVE A3-COD-POSTAL            TO A3-CP-FISCAL
                 WHEN OTHER
                    DISPLAY 'ERROR AL LEER  ZMDTDIR '
                    DISPLAY 'ZMDIR-ICUENTA ' ZMDIR-ICUENTA
                    MOVE SQLCODE          TO WS-SQLCODE
                    DISPLAY 'SQLCODE ' WS-SQLCODE
                    DISPLAY 'SE TERMINA EL PROGRAMA                '
                    PERFORM 999-ABORTA
              END-EVALUATE
           END-IF.
       033-FIN.
           EXIT.
      *
      *******************************************************************
      * BUSCA CLIENTE EN ACLICTA CON CUENTA                             *
      *******************************************************************
       034-BUSCA-CTE-ACLICTA.
           INITIALIZE DCLACLICTA
           MOVE ICUENTA OF DCLCUENTA        TO ICUENTA OF DCLACLICTA

              EXEC SQL
                 SELECT ICLIENTE
                   INTO :DCLACLICTA.ICLIENTE
                   FROM ACLICTA
                  WHERE ICUENTA   = :DCLACLICTA.ICUENTA
                   AND  IREGIMEN  = :WSC-TIPCTA
              END-EXEC

              EVALUATE SQLCODE
                 WHEN 0
                 WHEN +100
                   MOVE ICLIENTE OF DCLACLICTA
                                        TO ZMDIR-ICLIENTE OF DCLZMDTDIR
                 WHEN OTHER
                   DISPLAY 'ERROR SELECT ACLICTA               '
                   DISPLAY 'CUENTA : ' ICUENTA OF DCLACLICTA
                   MOVE SQLCODE              TO WS-SQLCODE
                   DISPLAY 'SQLCODE ' WS-SQLCODE
                   DISPLAY 'SE TERMINA EL PROGRAMA             '
                   PERFORM 999-ABORTA
              END-EVALUATE.
       034-FIN.
           EXIT.
      *
      *******************************************************************
      * SUSTITUYE LOS CARACTERES ESPECIALES POR CAMBIO DE ENCODING.     *
      *******************************************************************
       035-LIMPIA-NOMBRE.
                 INSPECT WS-A3-NOMBRE-AUX REPLACING
                         ALL LOW-VALUES BY SPACES
                         ALL '\00' BY SPACES   ALL '' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '\9C' BY SPACES   ALL '	' BY SPACES
                         ALL '\86' BY SPACES   ALL '' BY SPACES
                         ALL '\97' BY SPACES   ALL '\8D' BY SPACES
                         ALL '\8E' BY SPACES   ALL '' BY SPACES
                         ALL '' BY SPACES   ALL '
' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '\9D' BY SPACES   ALL '\85' BY SPACES
                         ALL '' BY SPACES   ALL '\87' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '\92' BY SPACES   ALL '\8F' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '\80' BY SPACES   ALL '\81' BY SPACES
                         ALL '\82' BY SPACES   ALL '\83' BY SPACES
                         ALL '\84' BY SPACES   ALL '
' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '\88' BY SPACES   ALL '\89' BY SPACES
                         ALL '\8A' BY SPACES   ALL '\8B' BY SPACES
                         ALL '\8C' BY SPACES   ALL '' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '\90' BY SPACES   ALL '\91' BY SPACES
                         ALL '' BY SPACES   ALL '\93' BY SPACES
                         ALL '\94' BY SPACES   ALL '\95' BY SPACES
                         ALL '\96' BY SPACES   ALL '' BY SPACES
                         ALL '\98' BY SPACES   ALL '\99' BY SPACES
                         ALL '\9A' BY SPACES   ALL '\9B' BY SPACES
                         ALL '' BY SPACES   ALL '' BY SPACES
                         ALL '\9E' BY SPACES   ALL '' BY SPACES
                         ALL ']' BY SPACES   ALL '\A0' BY '/'
                         ALL '\A1' BY 'A'      ALL '\A2' BY 'A'
                         ALL '\A3' BY 'A'      ALL '\A4' BY 'A'
                         ALL '\A5' BY 'A'      ALL '\A6' BY 'A'
                         ALL '\A7' BY SPACES   ALL '\A8' BY '\B9'
                         ALL '[' BY SPACES   ALL '\A9' BY 'E'
                         ALL '\AA' BY 'E'      ALL '\AB' BY 'E'
                         ALL '\AC' BY 'E'      ALL '\AD' BY 'I'
                         ALL '\AE' BY 'I'      ALL '\AF' BY 'I'
                         ALL '\B0' BY 'I'      ALL '\B1' BY SPACES
                         ALL '$' BY SPACES   ALL '^' BY SPACES
                         ALL '\B2' BY 'A'      ALL '\B3' BY 'A'
                         ALL '\B4' BY 'A'      ALL '\B5' BY 'A'
                         ALL '\B6' BY 'A'      ALL '\B7' BY 'A'
                         ALL '\B8' BY SPACES
                         ALL '|' BY '\B9'      ALL '_' BY SPACES
                         ALL '\BA' BY SPACES   ALL '\BB' BY 'E'
                         ALL '\BC' BY 'E'      ALL '\BD' BY 'E'
                         ALL '\BE' BY 'E'      ALL '\BF' BY 'I'
                         ALL '\C0' BY 'I'      ALL '\C1' BY 'I'
                         ALL '\C2' BY 'I'      ALL '`' BY SPACES
                         ALL '"' BY SPACES   ALL '#' BY '\B9'
                         ALL '@' BY 'A'      ALL "'" BY SPACES
                         ALL '\C3' BY '0'      ALL 'a' BY 'A'
                         ALL 'b' BY 'B'      ALL 'c' BY 'C'
                         ALL 'd' BY 'D'      ALL 'e' BY 'E'
                         ALL 'f' BY 'F'      ALL 'g' BY 'G'
                         ALL 'h' BY 'H'      ALL 'i' BY 'I'
                         ALL '\C4' BY SPACES   ALL '\C5' BY SPACES
                         ALL '\C6' BY SPACES   ALL '\C7' BY 'Y'
                         ALL '\C8' BY SPACES   ALL '\C9' BY SPACES
                         ALL '\CA' BY SPACES   ALL 'j' BY 'J'
                         ALL 'k' BY 'K'      ALL 'l' BY 'L'
                         ALL 'm' BY 'M'      ALL 'n' BY 'N'
                         ALL 'o' BY 'O'      ALL 'p' BY 'P'
                         ALL 'q' BY 'Q'      ALL 'r' BY 'R'
                         ALL '\CB' BY SPACES   ALL '\CC' BY SPACES
                         ALL '\CD' BY SPACES   ALL '\D0' BY SPACES
                         ALL '\CF' BY SPACES   ALL '~' BY SPACES
                         ALL '\D1' BY '\B9'      ALL 't' BY 'T'
                         ALL 's' BY 'S'      ALL 'v' BY 'V'
                         ALL 'u' BY 'U'      ALL 'x' BY 'X'
                         ALL 'w' BY 'W'      ALL '\D5' BY 'Y'
                         ALL 'y' BY 'Y'      ALL 'z' BY 'Z'
                         ALL '\D4' BY SPACES   ALL '\D7' BY SPACES
                         ALL '\D6' BY SPACES   ALL '\D9' BY SPACES
                         ALL '\D8' BY SPACES   ALL '\DB' BY SPACES
                         ALL '\DA' BY '\B9'      ALL '\DD' BY SPACES
                         ALL '\DC' BY SPACES   ALL '\DF' BY SPACES
                         ALL '\DE' BY SPACES   ALL '\E1' BY SPACES
                         ALL '\E0' BY SPACES   ALL '\E5' BY SPACES
                         ALL '\E4' BY SPACES   ALL '\E7' BY 'X'
                         ALL '\E6' BY SPACES   ALL '}' BY SPACES
                         ALL '{' BY SPACES   ALL '\E3' BY SPACES
                         ALL '\E2' BY SPACES   ALL '\E9' BY 'O'
                         ALL '\EA' BY 'O'      ALL '\EB' BY 'O'
                         ALL '\EC' BY 'O'      ALL '\ED' BY 'O'
                         ALL '\EE' BY '\B9'      ALL '\EF' BY 'U'
                         ALL '\F0' BY 'U'      ALL '\F1' BY 'U'
                         ALL '\F2' BY 'U'      ALL '\F3' BY 'Y'
                         ALL '\' BY '/'      ALL '\9F' BY SPACES
                         ALL '\F4' BY SPACES   ALL '\F5' BY 'O'
                         ALL '\F6' BY 'O'      ALL '\F7' BY 'O'
                         ALL '\F8' BY 'O'      ALL '\F9' BY 'O'
                         ALL '\FA' BY SPACES   ALL '\FB' BY 'U'
                         ALL '\FC' BY 'U'      ALL '\FD' BY 'U'
                         ALL '\FE' BY 'U'      ALL '\FF' BY SPACES.
       035-FIN.
           EXIT.
      * SDAT-39213F
      *
      ******************************************************************
      *   ABRE ARCHIVO CON EL DETALLE DE LOS CANJES
      ******************************************************************
       040-ABRE-CURSOR.
           MOVE 'N'                     TO WS-FIN-ARCHIVO.
           EXEC SQL
              OPEN C100-OPERASI
           END-EXEC.
           IF SQLCODE NOT = ZEROS AND
              SQLCODE NOT = 100
              DISPLAY 'ERROR AL ABRIR EL CURSOR C100-OPERASI'
              MOVE SQLCODE              TO WS-SQLCODE
              DISPLAY 'SQLCODE ' WS-SQLCODE
              DISPLAY 'SE TERMINA EL PROGRAMA                '
              PERFORM 999-ABORTA
           END-IF.
           IF SQLCODE = ZEROS
              PERFORM 055-LEE-CURSOR
           END-IF.
       040-FIN.
           EXIT.
      *
      ******************************************************************
      *   LEE UN REGISTRO DEL ARCHIVO DE ESTADOS DE CUENTA
      ******************************************************************
       050-LEE-CURSOR.
           MOVE WS-REGISTRO-ACT              TO WS-REGISTRO-ANT.
           MOVE ZEROS                        TO WS-REG-ANT-TOT-VTA-CTA.
           MOVE ZEROS                        TO WS-REG-ANT-TOT-TIT-CTA.
           MOVE ZEROS                        TO WS-REG-ANT-TOT-MNT-CTA.
           IF WS-FIN-ARCHIVO = 'F'
              MOVE 'S'                       TO WS-FIN-ARCHIVO
              MOVE HIGH-VALUES               TO IEMPR    OF DCLCUENTA
              MOVE HIGH-VALUES               TO IEMISORA OF DCLCONCEPT
              MOVE HIGH-VALUES               TO ISERIE   OF DCLCONCEPT
           ELSE
              PERFORM UNTIL (WS-REGISTRO-ACT NOT = WS-REGISTRO-ANT)
                         OR (WS-FIN-ARCHIVO      = 'F'            )
                 ADD  WS-REG-ACT-TOT-VTA-CTA TO WS-REG-ANT-TOT-VTA-CTA
                 ADD  WS-REG-ACT-TOT-TIT-CTA TO WS-REG-ANT-TOT-TIT-CTA
                 ADD  WS-REG-ACT-TOT-MNT-CTA TO WS-REG-ANT-TOT-MNT-CTA
                 PERFORM 055-LEE-CURSOR
              END-PERFORM
              PERFORM 052-MUEVE-DE-REGISTRO-ANT
           END-IF.
       050-FIN.
           EXIT.
      *
      ******************************************************************
      *    MUEVE UN REGISTRO AL REGISTRO ACTUAL PARA SU ACUMULACION
      ******************************************************************
       051-MUEVE-A-REGISTRO-ACT.
           MOVE IEMPR    OF DCLCUENTA   TO WS-REG-ACT-IEMPR.
           MOVE ISUCCASA OF DCLCUENTA   TO WS-REG-ACT-ISUCCASA.
           MOVE IPROM    OF DCLCUENTA   TO WS-REG-ACT-IPROM.
           MOVE ICUENTA  OF DCLCUENTA   TO WS-REG-ACT-ICUENTA.
           MOVE NOMBRE   OF DCLCUENTA   TO WS-REG-ACT-NOMBRE.
           MOVE NAPELL1  OF DCLCUENTA   TO WS-REG-ACT-NAPELL1.
           MOVE NAPELL2  OF DCLCUENTA   TO WS-REG-ACT-NAPELL2.
           MOVE IRFC     OF DCLCUENTA   TO WS-REG-ACT-IRFC.
           MOVE DCALLE   OF DCLCUENTA   TO WS-REG-ACT-DCALLE.
           MOVE DCOLON   OF DCLCUENTA   TO WS-REG-ACT-DCOLON.
           MOVE DCALLEP  OF DCLCUENTA   TO WS-REG-ACT-DCALLEP.
           MOVE DCOLONP  OF DCLCUENTA   TO WS-REG-ACT-DCOLONP.
           MOVE DPOBLA   OF DCLCUENTA   TO WS-REG-ACT-DPOBLA.
           MOVE DESTADO  OF DCLCUENTA   TO WS-REG-ACT-DESTADO.
           MOVE DPOBLAP  OF DCLCUENTA   TO WS-REG-ACT-DPOBLAP.
           MOVE DESTADOP OF DCLCUENTA   TO WS-REG-ACT-DESTADOP.
           MOVE IPOS     OF DCLCUENTA   TO WS-REG-ACT-IPOS.
           MOVE IPOSP    OF DCLCUENTA   TO WS-REG-ACT-IPOSP.
           MOVE IEMISORA OF DCLCONCEPT  TO WS-REG-ACT-IEMISORA.
           MOVE ISERIE   OF DCLCONCEPT  TO WS-REG-ACT-ISERIE.
           MOVE ICUPON   OF DCLCONCEPT  TO WS-REG-ACT-ICUPON.
      *
           MOVE 1                       TO WS-REG-ACT-TOT-VTA-CTA.
           MOVE CANT2    OF DCLOPERASI  TO WS-REG-ACT-TOT-TIT-CTA.
           MOVE CANT1    OF DCLOPERASI  TO WS-REG-ACT-TOT-MNT-CTA.
       051-FIN.
           EXIT.
      *
      ******************************************************************
      *    MUEVE DE REGISTRO ANTERIOR YA ACUMULADO A UN REGISTRO SALIDA
      ******************************************************************
       052-MUEVE-DE-REGISTRO-ANT.
           MOVE WS-REG-ANT-IEMPR        TO IEMPR    OF DCLCUENTA.
           MOVE WS-REG-ANT-ISUCCASA     TO ISUCCASA OF DCLCUENTA.
           MOVE WS-REG-ANT-IPROM        TO IPROM    OF DCLCUENTA.
           MOVE WS-REG-ANT-ICUENTA      TO ICUENTA  OF DCLCUENTA.
           MOVE WS-REG-ANT-NOMBRE       TO NOMBRE   OF DCLCUENTA.
           MOVE WS-REG-ANT-NAPELL1      TO NAPELL1  OF DCLCUENTA.
           MOVE WS-REG-ANT-NAPELL2      TO NAPELL2  OF DCLCUENTA.
           MOVE WS-REG-ANT-IRFC         TO IRFC     OF DCLCUENTA.
           MOVE WS-REG-ANT-DCALLE       TO DCALLE   OF DCLCUENTA.
           MOVE WS-REG-ANT-DCOLON       TO DCOLON   OF DCLCUENTA.
           MOVE WS-REG-ANT-DCALLEP      TO DCALLEP  OF DCLCUENTA.
           MOVE WS-REG-ANT-DCOLONP      TO DCOLONP  OF DCLCUENTA.
           MOVE WS-REG-ANT-DPOBLA       TO DPOBLA   OF DCLCUENTA.
           MOVE WS-REG-ANT-DESTADO      TO DESTADO  OF DCLCUENTA.
           MOVE WS-REG-ANT-DPOBLAP      TO DPOBLAP  OF DCLCUENTA.
           MOVE WS-REG-ANT-DESTADOP     TO DESTADOP OF DCLCUENTA.
           MOVE WS-REG-ANT-IPOS         TO IPOS     OF DCLCUENTA.
           MOVE WS-REG-ANT-IPOSP        TO IPOSP    OF DCLCUENTA.
           MOVE WS-REG-ANT-IEMISORA     TO IEMISORA OF DCLCONCEPT.
           MOVE WS-REG-ANT-ISERIE       TO ISERIE   OF DCLCONCEPT.
           MOVE WS-REG-ANT-ICUPON       TO ICUPON   OF DCLCONCEPT.
      *
           MOVE WS-REG-ANT-TOT-VTA-CTA  TO WS-TOT-VTA-CTA.
           MOVE WS-REG-ANT-TOT-TIT-CTA  TO WS-TOT-TIT-CTA.
           MOVE WS-REG-ANT-TOT-MNT-CTA  TO WS-TOT-MNT-CTA.
       052-FIN.
           EXIT.
      *
      ******************************************************************
      *   LEE UN REGISTRO DEL ARCHIVO DE ESTADOS DE CUENTA
      ******************************************************************
       055-LEE-CURSOR.
           MOVE SPACES                  TO WS-FIN-ARCHIVO.
           PERFORM UNTIL WS-FIN-ARCHIVO NOT = SPACES
              EXEC SQL
                 FETCH C100-OPERASI
                  INTO :DCLCUENTA.IEMPR,     :DCLCUENTA.ISUCCASA,
                       :DCLCUENTA.IPROM,     :DCLCUENTA.ICUENTA,
                       :DCLCUENTA.NOMBRE,    :DCLCUENTA.NAPELL1,
                       :DCLCUENTA.NAPELL2,
                       :DCLCUENTA.IRFC,
                       :DCLCUENTA.DCALLE,    :DCLCUENTA.DCOLON,
                       :DCLCUENTA.DCALLEP,   :DCLCUENTA.DCOLONP,
                       :DCLCUENTA.DPOBLA,    :DCLCUENTA.DESTADO,
                       :DCLCUENTA.DPOBLAP,   :DCLCUENTA.DESTADOP,
                       :DCLCUENTA.IPOS,      :DCLCUENTA.IPOSP,
                       :DCLOPERASI.CANT2,    :DCLOPERASI.CANT1,
                       :DCLCONCEPT.IEMISORA, :DCLCONCEPT.ISERIE,
                       :DCLCONCEPT.ICUPON
      *
      * FSW-1.0.0-I
                      ,:DCLCUENTA.IPERJUR,
                       :DCLCUENTA.ISUBCLAS,
                       :DCLCUENTA.IEMPR
      * FSW-1.0.0-F
      *
              END-EXEC
              IF SQLCODE NOT = ZEROS AND
                 SQLCODE NOT = 100
                 DISPLAY 'ERROR AL LEER  EL CURSOR C100-OPERASI'
                 MOVE SQLCODE              TO WS-SQLCODE
                 DISPLAY 'SQLCODE ' WS-SQLCODE
                 DISPLAY 'SE TERMINA EL PROGRAMA                '
                 PERFORM 999-ABORTA
              END-IF
              IF SQLCODE = 100
                 MOVE 'F'                  TO WS-FIN-ARCHIVO
              ELSE
                 MOVE 'N'                  TO WS-FIN-ARCHIVO
                 PERFORM 051-MUEVE-A-REGISTRO-ACT
              END-IF
           END-PERFORM.
       055-FIN.
           EXIT.
      *
      ******************************************************************
      *   CIERRA EL ARCHIVO CON LOS REGISTROS DE LOS CANJES
      ******************************************************************
       060-CIERRA-CURSOR.
           EXEC SQL
              CLOSE C100-OPERASI
           END-EXEC.
           IF SQLCODE NOT = ZEROS AND
              SQLCODE NOT = 100
              DISPLAY 'ERROR AL ABRIR EL CURSOR C100-OPERASI'
              MOVE SQLCODE              TO WS-SQLCODE
              DISPLAY 'SQLCODE ' WS-SQLCODE
              DISPLAY 'SE TERMINA EL PROGRAMA                '
              PERFORM 999-ABORTA
           END-IF.
       060-FIN.
           EXIT.
      *
      ******************************************************************
      *   CIERRA ARCHIVOS UTILIZADOS PARA LA GENERACION DE ACUSES
      ******************************************************************
       080-CIERRA-ARCHIVOS.
           WRITE REG-LISTADO-CBP      FROM R1-99-FIN     AFTER 2
           WRITE REG-LISTADO-BCM      FROM R1-99-FIN     AFTER 2
      *
           CLOSE ARCH-LISTADO-CBP
                 ARCH-LISTADO-BCM
                 ARCH-SECUENC-TXT
                 ARCH-INTEF-FUNDA.
       080-FIN.
           EXIT.
      *
      ******************************************************************
      *     ESCRIBE ENCABEZADO
      ******************************************************************
       160-ENCABEZADO.
           PERFORM 999-CONST-ENCA
           ADD 1                           TO R1-NUM-HOJA
           MOVE R1-NUM-HOJA                TO R1-01-NUM-HOJA
      *
           MOVE W000-FECHA-DD              TO R1-02-DIA
           MOVE T999-ABREV-MES(W000-FECHA-MM)
                                           TO R1-02-MES
           MOVE W000-FECHA-AA              TO R1-02-ANO
      *
           ACCEPT WS-HORA-SISTEMA        FROM TIME
           MOVE WS-HH                      TO R1-03-HORAS
           MOVE WS-MM                      TO R1-03-MINUTOS
           MOVE WS-SS                      TO R1-03-SEGUNDOS
      *
           MOVE WS-CUENTA-CARGO            TO R1-05-CTA-CARGO
      *
           MOVE IEMISORA OF DCLCONCEPT     TO R1-06-IEMISORA
           MOVE ISERIE   OF DCLCONCEPT     TO R1-06-ISERIE
           MOVE ICUPON   OF DCLCONCEPT     TO R1-06-ICUPON
           MOVE W000-FECHA-DD-INI          TO R1-06-DIA-INI-MES
           MOVE T999-ABREV-MES(W000-FECHA-MM-INI)
                                           TO R1-06-MES-INI-MES
           MOVE W000-FECHA-AA-INI          TO R1-06-ANO-INI-MES
           MOVE W000-FECHA-DD-FIN          TO R1-06-DIA-FIN-MES
           MOVE T999-ABREV-MES(W000-FECHA-MM-FIN)
                                           TO R1-06-MES-FIN-MES
           MOVE W000-FECHA-AA-FIN          TO R1-06-ANO-FIN-MES
           MOVE WS-CUENTA-ABONO            TO R1-06-CTA-ABONO
      *
           IF WS-EMP = 'CBP'
              WRITE REG-LISTADO-CBP      FROM R1-01-ENCA AFTER PAGE
              WRITE REG-LISTADO-CBP      FROM R1-02-ENCA
              WRITE REG-LISTADO-CBP      FROM R1-03-ENCA
              WRITE REG-LISTADO-CBP      FROM R1-04-ENCA
              WRITE REG-LISTADO-CBP      FROM R1-ESPACIOS
              WRITE REG-LISTADO-CBP      FROM R1-05-ENCA
              WRITE REG-LISTADO-CBP      FROM R1-06-ENCA
              WRITE REG-LISTADO-CBP      FROM R1-07-ENCA
              WRITE REG-LISTADO-CBP      FROM R1-08-ENCA
              WRITE REG-LISTADO-CBP      FROM R1-07-ENCA
           ELSE
              WRITE REG-LISTADO-BCM      FROM R1-01-ENCA AFTER PAGE
              WRITE REG-LISTADO-BCM      FROM R1-02-ENCA
              WRITE REG-LISTADO-BCM      FROM R1-03-ENCA
              WRITE REG-LISTADO-BCM      FROM R1-04-ENCA
              WRITE REG-LISTADO-BCM      FROM R1-ESPACIOS
              WRITE REG-LISTADO-BCM      FROM R1-05-ENCA
              WRITE REG-LISTADO-BCM      FROM R1-06-ENCA
              WRITE REG-LISTADO-BCM      FROM R1-07-ENCA
              WRITE REG-LISTADO-BCM      FROM R1-08-ENCA
              WRITE REG-LISTADO-BCM      FROM R1-07-ENCA
           END-IF.
           MOVE  10                        TO R1-NUM-LIN.
       160-FIN.
           EXIT.
      *
      ******************************************************************
      *    CONSTRUYE ENCABEZADOS
      ******************************************************************
             EXEC SQL
                  INCLUDE  ZMWBP000
             END-EXEC.
      *
      ******************************************************************
      *    RUTINA PARA OBTENER FECHAS PROCESO
      ******************************************************************
             EXEC SQL
                  INCLUDE  ZMWBP070
             END-EXEC.
      *
      ******************************************************************
      *    OBTIENE EL CASILLERO PARA CONTROL-D
      *9800-OBT-CASILLERO
      ******************************************************************
            EXEC SQL
                INCLUDE ZMWBP074
            END-EXEC.
      *
      ******************************************************************
      *9886-ALINEA-STRING-IZQ.
      *9887-ALINEA-STRING-DER.
      ******************************************************************
           EXEC SQL
               INCLUDE ZMWSC015
           END-EXEC.
      *
      ******************************************************************
      *  ABORTA EL PROGRAMA
      ******************************************************************
       999-ABORTA.
           EXEC SQL
                ROLLBACK
           END-EXEC.
      *
           MOVE  16                     TO RETURN-CODE.
           DISPLAY 'TERMINACION ANORMAL DEL PROGRAMA'
      *
           STOP RUN.
       999-FIN.
           EXIT.
      *
      ******************************************************************
      *  REGRESA EL CONTROL AL PROGRAMA LLAMADOR
      ******************************************************************
       999-TERMINA.
           EXEC SQL
                COMMIT
           END-EXEC.
      *
           STOP RUN.
       999-FIN.
           EXIT.
      *
      ******************************************************************
      *  TERMINA PROGRAMA ZM4DJ097                                     *
      ******************************************************************
