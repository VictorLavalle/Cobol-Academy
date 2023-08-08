       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID. ZM3CB001.
       AUTHOR.
      *----
      *    AUTOR       :
      *    FECHA       : ENERO 2018
      *    OBJETIVO    : SE REALIZA CRUCE POR NO. DE CLIENTE ENTRE EL
      *                  ARCHIVO DEL SAT Y EL ARCHIVO
      *                  PERSONAS (DESCARGA DE LA TABLA PEDT001) PARA
      *                  OBTENER DATOS ADMINISTRATIVOS
      *                  TAMBIEN BUSCA EL PRODUCTO, DIVISA, EMPRESA
      *                  Y TIPO DE PERSONA CORRESPONDIENTE AL SAT
      *                  *********CASA DE BOLSA**********
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370.
       OBJECT-COMPUTER.
           IBM-370.
      *
       INPUT-OUTPUT SECTION.
      *----
       FILE-CONTROL.
      *----
      *    ENTRADAS
      *
      *    SA-SAT1         ES EL INVENTARIO
      *    SA-CLI1         ES EL ARCHIVO DE PERSONAS
      *----
           SELECT SA-SAT1             ASSIGN TO MISPRODU
                                      FILE STATUS IS STATUS-INV1.
           SELECT SA-CLI1             ASSIGN TO MISCLIEN
                                      FILE STATUS IS STATUS-CLI1.
      *----
      *    SALIDAS
           SELECT SA-REP2            ASSIGN TO REPORTE2
                                      FILE STATUS IS STATUS-REP2.
      /
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
       FD  SA-SAT1
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           RECORD CONTAINS 201 CHARACTERS
           LABEL RECORD STANDARD
           DATA RECORD WS-REG-SAT.
       01  WS-REG-SAT.
           05 WS-SAT-ANIO            PIC X(04).
           05 WS-SAT-CONTRATO        PIC X(30).
           05 WS-SAT-SECUENCI        PIC X(10).
           05 WS-SAT-PRODUCTO        PIC X(10).
           05 WS-SAT-DIVISA          PIC X(03).
           05 WS-SAT-TPODATO         PIC X(02).
           05 WS-SAT-TABLA.
              10 WS-SAT-MESES           OCCURS 12 TIMES.
                 15 WS-SAT-IMPORTE         PIC S9(15)V99    COMP-3.
           05 WS-SAT-BANCO           PIC X(04).
           05 WS-SAT-CTADOMI         PIC X(30).
      *
       FD  SA-CLI1
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           LABEL RECORD STANDARD
           RECORD CONTAINS 469 CHARACTERS
           DATA RECORD REG-CLI1.
       01  REG-CLI1.
           05  CAT-CVE-IDEN               PIC X(1).
           05  CAT-CVE-EMP                PIC X(4).
           05  CAT-RFC                    PIC X(13).
           05  CAT-CURP                   PIC X(18).
           05  CAT-NOMBRE                 PIC X(150).
           05  CAT-DOMICILIO              PIC X(160).
           05  CAT-TPO-PER                PIC X(01).
           05  CAT-CONTRATO               PIC X(30).
           05  CAT-SECUENCIA              PIC X(10).
           05  CAT-APLICACI               PIC X(10).
           05  CAT-TPO-REGIMEN            PIC X(01).
           05  CAT-FIDEICOMISO            PIC X(20).
           05  CAT-COTITU                 PIC X(01).
           05  CAT-MUNICIPIO              PIC X(30).
           05  CAT-ESTADO                 PIC X(20).
      *
       FD  SA-REP2
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           LABEL RECORD STANDARD
           RECORD CONTAINS 626 CHARACTERS
           DATA RECORD REG-REP2.
       01  REG-REP2.
           05 WS-REP-ANIO            PIC X(04).
           05 WS-REP-CONTRATO        PIC X(30).
           05 WS-REP-SECUENCI        PIC X(10).
           05 WS-REP-PRODUCTO        PIC 9(02).
           05 WS-REP-DIVISA          PIC 9(01).
           05 WS-REP-RFC             PIC X(13).
           05 WS-REP-CURP            PIC X(18).
           05 WS-REP-NOM-COMP        PIC X(150).
           05 WS-REP-DOMICILIO       PIC X(160).
           05 WS-REP-PERSONA.
              10 WS-REP-TPO-PER      PIC X(01).
           05 WS-REP-TPODATO         PIC X(02).
           05 WS-REP-TABLA.
              10 WS-REP-MESES           OCCURS 12 TIMES.
                 15 WS-REP-IMPORTE         PIC S9(15)V99    COMP-3.
           05 WS-REP-BANCO-SAT       PIC 9(05).
           05 WS-REP-PRODMIS         PIC X(10).
           05 WS-REP-DIVMIS          PIC X(03).
           05 WS-REP-CODDIV          PIC X(01).
           05 WS-REP-SECTORMIS       PIC X(03).
           05 WS-REP-BCOMIS          PIC X(04).
           05 WS-REP-CTADOMI         PIC X(30).
           05 WS-REP-FIDEICOMISO     PIC X(20).
           05 WS-REP-COTITU          PIC X(01).
           05 WS-REP-MUNICIPIO       PIC X(30).
           05 WS-REP-ESTADO          PIC X(20).
      *
      *
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
      *    CONSTANTES                                                 *
      *---------------------------------------------------------------*
       01 CON-PROGRAMA                 PIC X(08) VALUE 'ZM3CB001'.
       01 CON-FILE-STATUS              PIC X(12) VALUE 'FILE STATUS='.
      *
      *          DEFINICION DE FILES-STATUS DE LOS ARCHIVOS           *
      *---------------------------------------------------------------*
      *
       01 STATUS-INV1                       PIC X(02) VALUE '00'.
          88 FS-INV1-OK                               VALUE '00'.
          88 FS-INV1-FIN                              VALUE '10'.
      *
       01 STATUS-CLI1                       PIC X(02) VALUE '00'.
          88 FS-CLI1-OK                               VALUE '00'.
          88 FS-CLI1-FIN                              VALUE '10'.
      *
       01 STATUS-REP2                       PIC X(02) VALUE '00'.
          88 FS-REP2-OK                               VALUE '00'.
      *
      *
      *----
      *    DEFINICION DE SWITCHES DE TRABAJO
      *----
       01 S500-LEE-INV1                     PIC X(01) VALUE SPACES.
       01 S500-LEE-CLI1                     PIC X(01) VALUE SPACES.
       01 S500-LOOP                         PIC 9(08) VALUE ZEROS.
      *----
      *    ACUMULADORES
      *----
       01 A990-CIF-CTL.
          05 A990-REG-SI-ACT                PIC 9(11).
          05 A990-REG-NO-ACT                PIC 9(11).
          05 A990-REG-LEI-INV1              PIC 9(11).
          05 A990-REG-LEI-CLI1              PIC 9(11).
          05 A990-REG-ESC-REP2              PIC 9(11).
          05 A990-CIFRA                     PIC ZZZ,ZZZ,ZZ9.
      *----------------------------------------------------------------*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *IDENTIDADES
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
       01 ID-INV-LEI.
          05 ID-CONTRATO-INV-LEI           PIC X(30).
          05 ID-SECUENCIA-INV-LEI          PIC X(10).
       01 ID-INV-ANT                       PIC X(40).
       01 ID-INV-PROC                      PIC X(40).
      *
       01 ID-CLI-LEI.
          05 ID-CONTRATO-CLI-LEI           PIC X(30).
          05 ID-SECUENCIA-CLI-LEI          PIC X(10).
       01 ID-CLI-ANT                       PIC X(40).
       01 ID-CLI-PROC                      PIC X(40).
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *DEFINICION DE TABLAS DE MEMORIA PARA LOS TIPOS DE DIVISA
      *Y PRODUCTOS
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01 DEFINICION-TABLAS.
          05 T001-DIVISAS OCCURS 100 TIMES.
             10 T001-CVE-DIV   PIC X(03).
             10 T001-DIV-SAT   PIC X(01).
          05 I001-I                         PIC 9(03) VALUE 0.
      *
          05 T002-PRODUCTO OCCURS 500 TIMES.
             10 T002-CVE-PRO   PIC X(02).
             10 T002-CVE-PROFI PIC X(08).
             10 T002-PRO-SAT   PIC X(02).
          05 I002-I                         PIC 9(03) VALUE 0.
      *
          05 T003-EMPRESA  OCCURS 100 TIMES.
             10 T003-CVE-EMP   PIC X(04).
             10 T003-EMP-SAT   PIC X(05).
          05 I003-I                         PIC 9(03) VALUE 0.
      *
          05 T004-SECTOR   OCCURS 100 TIMES.
             10 T004-CVE-SEC   PIC X(03).
             10 T004-SEC-SAT   PIC X(01).
          05 I004-I                         PIC 9(03) VALUE 0.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *DEFINICION DE VARIABLES DE TRABAJO
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01 DEFINICION-VARIABLES.
          05 W027-PER-SECTOR.
             10 W027-TPO-PER                PIC X(01).
             10 W027-SECTOR                 PIC X(02).
          05 S025-PROD                      PIC X(01).
          05 S026-DIV                       PIC X(01).
          05 S027-SEC                       PIC X(01).
          05 S028-EMP                       PIC X(01).
          05 S550-LEE-PARAM                 PIC X(01).
          05 WS-SAT-PARAM.
             10 WS-SAT-TPO                  PIC X(03).
             10 WS-SAT-FILLER               PIC X(12).
      *
          05 WS-SAT-DIV-PARAM.
             10 WS-SAT-DIV                  PIC X(03).
             10 WS-SAT-FILLER1              PIC X(01).
             10 WS-SAT-CVE-MIS              PIC X(03).
             10 WS-SAT-FILLER2              PIC X(08).
          05 WS-SAT-DIV-DATOS.
             10 WS-SAT-CVE-DIV              PIC X(01).
             10 WS-SAT-FILLER3              PIC X(59).
      *
          05 WS-SAT-PRO-PARAM.
             10 WS-SAT-PRO                  PIC X(03).
             10 WS-SAT-FILLER4              PIC X(01).
             10 WS-SAT-PRO-MIS              PIC XX.
             10 WS-SAT-PRO-FIL              PIC X(08).
             10 WS-SAT-FILLER5              PIC X(05).
          05 WS-SAT-PRO-DATOS.
             10 WS-SAT-PRO-SAT              PIC X(02).
             10 WS-SAT-FILLER6              PIC X(58).
      *
          05 WS-SAT-EMP-PARAM.
             10 WS-SAT-EMP                  PIC X(03).
             10 WS-SAT-FILLER7              PIC X(01).
             10 WS-SAT-EMP-MIS              PIC X(04).
             10 WS-SAT-FILLER8              PIC X(07).
          05 WS-SAT-EMP-DATOS.
             10 WS-SAT-EMP-SAT              PIC X(05).
             10 WS-SAT-FILLER9              PIC X(55).
      *
          05 WS-SAT-SEC-PARAM.
             10 WS-SAT-SEC                  PIC X(03).
             10 WS-SAT-FILLE10              PIC X(01).
             10 WS-SAT-SEC-MIS              PIC X(03).
             10 WS-SAT-FILLE11              PIC X(08).
          05 WS-SAT-SEC-DATOS.
             10 WS-SAT-SEC-SAT              PIC X(01).
             10 WS-SAT-FILLE12              PIC X(59).
      *
      *---------------------------------------------------------------*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       001-PROCESO SECTION.
           PERFORM 010-INICIALIZA
           PERFORM 020-PRINCIPAL UNTIL S500-LEE-INV1   = '1'
           PERFORM 030-TERMINA
           STOP RUN.
      /
      ******************************************************************
      *----
      *    INICIO (INICIALIZA VARIABLES Y ABRE ARCHIVOS)
      *----
       010-INICIALIZA SECTION.
           MOVE ZEROES                      TO A990-REG-LEI-INV1
                                               A990-REG-LEI-CLI1
                                               A990-REG-ESC-REP2
                                               A990-REG-SI-ACT
                                               A990-REG-NO-ACT
                                               A990-CIFRA
           MOVE LOW-VALUES                  TO ID-CLI-LEI
                                               ID-CLI-ANT
                                               ID-INV-LEI
                                               ID-INV-ANT
           MOVE '0'                         TO S500-LEE-INV1
                                               S500-LEE-CLI1
      *-
           OPEN INPUT SA-SAT1
           IF NOT FS-INV1-OK
              DISPLAY 'ERROR AL ABRIR ARCHIVO DE INVENTARIO'
              DISPLAY 'FILE STATUS = ' STATUS-INV1
              MOVE 16                  TO RETURN-CODE
           END-IF
      *-
           OPEN INPUT SA-CLI1
           IF NOT FS-CLI1-OK
              DISPLAY 'ERROR AL ABRIR ARCHIVO DE PERSONAS'
              DISPLAY 'FILE STATUS = ' STATUS-CLI1
              MOVE 16                  TO RETURN-CODE
           END-IF
      *-
           OPEN OUTPUT SA-REP2
           IF NOT FS-REP2-OK
              DISPLAY 'ERROR AL ABRIR ARCHIVO DE SALIDA'
              DISPLAY 'FILE STATUS = ' STATUS-REP2
              MOVE 16                  TO RETURN-CODE
           END-IF
      *-
           PERFORM 500-LEE-INV1
           PERFORM 505-LEE-CLI1
           EXIT.
       020-PRINCIPAL SECTION.
            ADD 1 TO S500-LOOP
            IF S500-LOOP = 50000 OR S500-LOOP = 100000
            OR S500-LOOP = 150000 OR S500-LOOP = 250000
               DISPLAY 'LLAVE DE INV ' ID-INV-LEI
               DISPLAY 'LLAVE DE CLI ' ID-CLI-LEI
            END-IF
            IF ID-INV-LEI = ID-CLI-LEI
               MOVE ID-INV-LEI          TO ID-INV-PROC
               PERFORM UNTIL ID-INV-LEI NOT = ID-INV-PROC
                  MOVE WS-SAT-ANIO         TO WS-REP-ANIO
                  MOVE WS-SAT-CONTRATO     TO WS-REP-CONTRATO
                  MOVE WS-SAT-SECUENCI     TO WS-REP-SECUENCI
                  MOVE WS-SAT-CTADOMI      TO WS-REP-CTADOMI
      *
                  PERFORM 025-BUSCA-PROD
      *
                  PERFORM 026-BUSCA-DIV
      *
                  MOVE WS-SAT-TPODATO      TO WS-REP-TPODATO
                  MOVE WS-SAT-TABLA        TO WS-REP-TABLA
                  MOVE CAT-RFC             TO WS-REP-RFC
                  MOVE CAT-CURP            TO WS-REP-CURP
                  MOVE CAT-NOMBRE          TO WS-REP-NOM-COMP
                  MOVE CAT-DOMICILIO       TO WS-REP-DOMICILIO
                  MOVE CAT-TPO-PER         TO W027-TPO-PER
                  MOVE SPACES              TO W027-SECTOR
                  MOVE CAT-FIDEICOMISO     TO WS-REP-FIDEICOMISO
                  MOVE CAT-COTITU          TO WS-REP-COTITU
                  MOVE CAT-MUNICIPIO       TO WS-REP-MUNICIPIO
                  MOVE CAT-ESTADO          TO WS-REP-ESTADO
      *
                  PERFORM 027-BUSCA-SECTOR
      *
                  PERFORM 028-BUSCA-BANCO
      *
                  ADD 1                    TO A990-REG-SI-ACT
                  PERFORM 600-GRABA-REP2
                  PERFORM 500-LEE-INV1
               END-PERFORM
               MOVE ID-CLI-LEI          TO ID-CLI-PROC
               PERFORM UNTIL ID-CLI-LEI NOT = ID-CLI-PROC
                  PERFORM 505-LEE-CLI1
               END-PERFORM
            ELSE
               IF ID-INV-LEI < ID-CLI-LEI
                  MOVE ID-INV-LEI          TO ID-INV-PROC
                  PERFORM UNTIL ID-INV-LEI NOT = ID-INV-PROC
                     MOVE WS-SAT-ANIO      TO WS-REP-ANIO
                     MOVE WS-SAT-CONTRATO  TO WS-REP-CONTRATO
                     MOVE SPACES           TO WS-REP-COTITU
                     MOVE WS-SAT-SECUENCI  TO WS-REP-SECUENCI
                     MOVE WS-SAT-CTADOMI   TO WS-REP-CTADOMI
      *
                     PERFORM 025-BUSCA-PROD
      *
                     PERFORM 026-BUSCA-DIV
      *
                     MOVE WS-SAT-TPODATO   TO WS-REP-TPODATO
                     MOVE WS-SAT-TABLA     TO WS-REP-TABLA
      *
                     PERFORM 028-BUSCA-BANCO
      *
                     MOVE SPACES           TO WS-REP-RFC
                     MOVE SPACES           TO WS-REP-CURP
                                              WS-REP-NOM-COMP
                                              WS-REP-DOMICILIO
                                              WS-REP-TPO-PER
                                              WS-REP-SECTORMIS
                                              WS-REP-FIDEICOMISO
                                              WS-REP-COTITU
                                              WS-REP-MUNICIPIO
                                              WS-REP-ESTADO
                     ADD 1                    TO A990-REG-NO-ACT
                     PERFORM 600-GRABA-REP2
                     PERFORM 500-LEE-INV1
                  END-PERFORM
               ELSE
                  IF ID-INV-LEI > ID-CLI-LEI
                      PERFORM 505-LEE-CLI1
                  END-IF
               END-IF
            END-IF
           EXIT.
      /
       025-BUSCA-PROD  SECTION.
           MOVE WS-SAT-PRODUCTO         TO WS-REP-PRODUCTO
           MOVE WS-SAT-PRODUCTO         TO WS-REP-PRODMIS
           EXIT.
      /
       026-BUSCA-DIV   SECTION.
           MOVE '0'                     TO WS-REP-DIVISA
           MOVE '0'                     TO WS-REP-CODDIV
           MOVE WS-SAT-DIVISA           TO WS-REP-DIVMIS
           EXIT.
      /
       027-BUSCA-SECTOR  SECTION.
           MOVE W027-PER-SECTOR         TO WS-REP-PERSONA
           MOVE W027-PER-SECTOR         TO WS-REP-SECTORMIS
           EXIT.
      /
       028-BUSCA-BANCO   SECTION.
           MOVE '0074'                  TO WS-REP-BANCO-SAT
           MOVE WS-SAT-BANCO            TO WS-REP-BCOMIS
           EXIT.
      /
       030-TERMINA SECTION.
      *
           CLOSE SA-SAT1
           IF NOT FS-INV1-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DE INVENTARIO'
              DISPLAY 'FILE STATUS : ' STATUS-INV1
              MOVE 16                       TO RETURN-CODE
           END-IF
      *
           CLOSE SA-CLI1
           IF NOT FS-CLI1-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DE PERSONAS'
              DISPLAY 'FILE STATUS : ' STATUS-CLI1
              MOVE 16                       TO RETURN-CODE
           END-IF
           CLOSE SA-REP2
           IF NOT FS-REP2-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO DE REPORTE'
              DISPLAY 'FILE STATUS : ' STATUS-REP2
              MOVE 16                       TO RETURN-CODE
           END-IF
      *
           PERFORM 990-CIF-CTL
           EXIT.
      /
      ******************************************************************
      /
      ******************************************************
      *   LECTURA DE ARCHIVOS
      ******************************************************
       500-LEE-INV1 SECTION.
           MOVE ID-INV-LEI                  TO ID-INV-ANT
           READ SA-SAT1 AT END
              MOVE '1'                      TO S500-LEE-INV1
              MOVE HIGH-VALUES              TO ID-INV-LEI
           END-READ
           IF NOT FS-INV1-OK AND NOT FS-INV1-FIN
              DISPLAY 'ERROR AL LEER ARCHIVO DE INVENTARIO'
              DISPLAY 'FILE STATUS : ' STATUS-INV1
              MOVE 16                TO RETURN-CODE
           END-IF
           IF NOT FS-INV1-FIN
              MOVE WS-SAT-CONTRATO          TO ID-CONTRATO-INV-LEI
              MOVE WS-SAT-SECUENCI          TO ID-SECUENCIA-INV-LEI
              IF ID-INV-LEI < ID-INV-ANT
                 DISPLAY '********************************************'
                 DISPLAY '*  ARCHIVO MIS PRODUCTOS NO ESTA ORDENADO  *'
                 DISPLAY '*  NO. CONTRATO ANTERIOR: ', ID-INV-ANT
                 DISPLAY '*  NO. CONTRATO ACTUAL  : ', ID-INV-LEI
                 DISPLAY '********************************************'
                 MOVE 16                    TO RETURN-CODE
                 PERFORM 030-TERMINA
              ELSE
                 ADD 1                      TO A990-REG-LEI-INV1
              END-IF
           END-IF
           EXIT.
      /
       505-LEE-CLI1 SECTION.
           MOVE ID-CLI-LEI                  TO ID-CLI-ANT
           READ SA-CLI1  AT END
              MOVE '1'                      TO S500-LEE-CLI1
              MOVE HIGH-VALUES              TO ID-CLI-LEI
           END-READ
           IF NOT FS-CLI1-OK AND NOT FS-CLI1-FIN
              DISPLAY 'ERROR AL LEER CATALOGO DE APLICATIVO'
              DISPLAY 'FILE STATUS : ' STATUS-CLI1
              MOVE 16                TO RETURN-CODE
           END-IF
           IF NOT FS-CLI1-FIN
              MOVE CAT-CONTRATO             TO ID-CONTRATO-CLI-LEI
              MOVE CAT-SECUENCIA            TO ID-SECUENCIA-CLI-LEI
              IF ID-CLI-LEI < ID-CLI-ANT
                 DISPLAY '********************************************'
                 DISPLAY '*  CATALOGO DEL APLICATIVO NO ESTA ORDENADO*'
                 DISPLAY '*  NO.CONTRATO ANTERIOR: ', ID-CLI-ANT
                 DISPLAY '*  NO.CONTRATO ACTUAL  : ', ID-CLI-LEI
                 DISPLAY '********************************************'
                 MOVE 16                    TO RETURN-CODE
                 PERFORM 030-TERMINA
              ELSE
                 ADD 1                      TO A990-REG-LEI-CLI1
              END-IF
           END-IF
           EXIT.
      /
       600-GRABA-REP2 SECTION.
           WRITE   REG-REP2
           IF NOT FS-REP2-OK
              DISPLAY 'ERROR AL ESCRIBIR ARCHIVO DE SALIDA'
              DISPLAY 'FILE STATUS: ' STATUS-REP2
              MOVE 16                    TO RETURN-CODE
           ELSE
              ADD 1                         TO  A990-REG-ESC-REP2
           END-IF
           EXIT.
      /
      *----
      *    CIFRAS DE CONTROL
      *----
       990-CIF-CTL SECTION.
           MOVE A990-REG-LEI-INV1           TO A990-CIFRA
           DISPLAY CON-PROGRAMA,
           'REGISTROS LEIDOS INVENTARIO MIS PRODUCTOS:',
           A990-CIFRA
      *
           MOVE A990-REG-LEI-CLI1           TO A990-CIFRA
           DISPLAY CON-PROGRAMA,
           'REGISTROS LEIDOS CATALOGO APLICATIVO     :',
           A990-CIFRA
      *
           MOVE A990-REG-ESC-REP2           TO A990-CIFRA
           DISPLAY CON-PROGRAMA,
           'REGISTROS ESCRITOS                       :',
           A990-CIFRA
      *
           MOVE A990-REG-SI-ACT             TO A990-CIFRA
           DISPLAY CON-PROGRAMA,
           'REGISTROS CON DATOS ADMINISTRATIVOS      :',
           A990-CIFRA
      *
           MOVE A990-REG-NO-ACT             TO A990-CIFRA
           DISPLAY CON-PROGRAMA,
           'REGISTROS SIN DATOS ADMINISTRATIVOS      :',
           A990-CIFRA
      *
           EXIT.
      *-----------------------------------------------------------------
