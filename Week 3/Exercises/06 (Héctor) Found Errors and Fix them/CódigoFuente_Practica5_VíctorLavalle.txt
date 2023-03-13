      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.     CBL0001
       AUTHOR.         Victor Lavalle.
       DATE-WRITTEN    FEBRERO 2022.

      *________________________________________________________________*
      *                                                                *
      *                     "PRACTICA 6"                               *
      *                                                                *
      * Instrucciones: Encontrar los errores y corregirlos             *
      *                                                                *
      *                                                                *
      *                                                                *
      *________________________________________________________________*


      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTREC.



      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01 PRINT-REC.
          05 WSV-NOMBRE-S         PIC X(15).
          05 FILLER               PIC X(03)         VALUE SPACES.
          05 WSV-ACCT-SALDO-S     PIC $$,$$$,$$9.99.
          05 FILLER               PIC X(03)         VALUE SPACES.
          05 WSV-ESTADO-S         PIC X(20).


      *Este es el archivo de entrada
       FD  ACCT-REC RECORDING MODE F.
       01 ACCT-FIELDS.
          05 WSV-ACCT-ID          PIC X(08).
          05 WSV-ACCT-LIMITE      PIC S9(07)V99 COMP-3.
          05 WSV-ACCT-SALDO       PIC S9(07)V99 COMP-3.
          05 WSV-APELLIDO-P       PIC X(20).
          05 WSV-NOMBRE           PIC X(15).
          05 WSV-DIRECCION.
             10 WSV-CALLE         PIC X(25).
             10 WSV-CIUDAD        PIC X(20).
             10 WSV-ESTADO        PIC X(15).
          05 WSV-RESERVED         PIC X(07).
          05 WSV-COMENTARIOS      PIC X(50).


       WORKING-STORAGE SECTION.
      ********************************************
      * Banderas                                 *
      ********************************************
       01 FLAGS.
          05 BANDERA-FILE         PIC X             VALUE SPACE.
             88 FIN-FILE                            VALUE 'Y'.
             88 NO-FIN-FILE                         VALUE 'N'.
          05 BANDERA-VIRGINIA     PIC X             VALUE SPACE.
             88 SI-VIRGINIA                         VALUE 'Y'.
             88 NO-VIRGINIA                         VALUE 'N'.
          05 BANDERA-REGALO       PIC X             VALUE SPACE.
             88 NO-REGALO                           VALUE 'Y'.
             88 REGALO-BOLETOS                      VALUE 'N'.

      ********************************************
      * Constantes                               *
      ********************************************
       01 CONSTANTES.
          05 WSC-VIRGINIA         PIC X(08)         VALUE 'VIRGINIA'.
          05 WSC-VIRGINIA2        PIC X(08)         VALUE 'Virginia'.
          05 WSC-1                PIC 9(01)         VALUE 1.
          05 WSC-3                PIC 9(01)         VALUE 3.
          05 WSC-20000            PIC 9(05)         VALUE 20000.

      ********************************************
      * Contadores/Acumuladores                  *
      ********************************************
       01 CONTADORES.
          05 WSV-CONTADORBOLETOS  PIC 9(01).



      ********************************************
      * Contadores/Acumuladores                  *
      ********************************************
       01 VARIABLES.
          05 WSV-SALDOMOMENTO     PIC 9(05).


      ********************************************
      * Formato de impreion del reporte          *
      ********************************************
      *
      ******************
      * Salto de linea *
      ******************
       01 BREAKLINE.
          05 BLANK-SPACE          PIC X(50)         VALUE ALL " ".

      ********************************************
      * Encabezado 1 con nombre de los atributos *
      ********************************************
       01 HEAD-1.
          05 FILLER               PIC X(22)         VALUE
                'Elabora: Víctor'.
          05 FILLER               PIC X(20)         VALUE SPACES.


      ********************************************
      * Encabezado 2 formato de separacion       *
      ********************************************
       01 HEAD-VIR.
          05 FILLER               PIC X(28)         VALUE
                '-------- AREA DE VIRGINIA --'.
          05 FILLER               PIC X(05)         VALUE '-----'.


      ********************************************
      * Encabezado que mostrar que ganó boleto   *
      ********************************************
       01 HEAD-BOLETOS.
          05 FILLER               PIC X(28)         VALUE
                'FELICIDADES TIENES UN BOLETO'.




      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      ******************************************************
      * Parrafo para abrir  el archivo de entrada           *
      ******************************************************
       ABRO-ARCHIVOS.
           OPEN INPUT ACCT-REC.
           OPEN OUTPUT PRINT-LINE.



      ******************************************************
      * Parrafo que contiene la lógica principal           *
      ******************************************************
       PROCESO-PRINCIPAL.

           SET NO-FIN-FILE TO TRUE
           SET NO-VIRGINIA TO TRUE
      *
      * Se escribe el encabezado del reporte
      *
           PERFORM ESCRIBO-ENCABEZADO
      *
      * Se lee el archivo de entrada y se procesa
      *
           PERFORM LEE-ARCHIVO
           PERFORM UNTIL FIN-FILE

                   PERFORM ESCRIBO-VIRGINIA

                   PERFORM VERIFICAR-IMPRESION

                   PERFORM LEE-ARCHIVO

           END-PERFORM.


      ******************************************************
      * Parrafo para cerrar el archivo de entrada          *
      *  y finalizar el programa                           *
      ******************************************************
       CIERRO-Y-ACABO.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           GOBACK.


      ******************************************************
      * Parrafo para leer  el archivo de entrada           *
      ******************************************************
       LEE-ARCHIVO.
           READ ACCT-REC
           AT END
              SET FIN-FILE TO TRUE
           END-READ.


      ******************************************************
      * Parrafo para escribir el reporte                   *
      *                                                    *
      ******************************************************
       ESCRIBO-ARCHIVO.

           MOVE WSV-NOMBRE TO WSV-NOMBRE-S
           MOVE WSV-ESTADO TO WSV-ESTADO-S
           MOVE WSV-ACCT-SALDO TO WSV-ACCT-SALDO-S

           WRITE PRINT-REC AFTER ADVANCING 1 LINES.



      ************************************************************
      * Parrafo que calsifica solo si son del estado de Virginia *
      ************************************************************
       ESCRIBO-VIRGINIA.
           EVALUATE WSV-ESTADO

           WHEN WSC-VIRGINIA2
                SET SI-VIRGINIA TO TRUE
                PERFORM ESCRIBO-ARCHIVO

           WHEN OTHER
                SET NO-VIRGINIA TO TRUE

           END-EVALUATE.


      ******************************************************
      * Parrafo para escribir boletos                      *
      *                                                    *
      ******************************************************
       ESCRIBO-BOLETOS.
           WRITE PRINT-REC FROM HEAD-BOLETOS AFTER ADVANCING 1 LINES.



      ******************************************************
      * Parrafo que valida que solo se impriman cuando     *
      * registros cuando el el saldo es mayor a 20000      *
      * y si son del estado de Virginia                     *
      ******************************************************

       VERIFICAR-IMPRESION.
           IF WSV-ACCT-SALDO >= WSC-20000
              IF SI-VIRGINIA
                 PERFORM BOLETOS-VIRGINIA-REGALO
              END-IF
           END-IF
           SET REGALO-BOLETOS TO TRUE.


      ******************************************************
      * Parrafo para escribir ganador de boleto            *
      *                                                    *
      ******************************************************
       BOLETOS-VIRGINIA-REGALO.

           INITIALIZE WSV-SALDOMOMENTO
           INITIALIZE WSV-CONTADORBOLETOS

           PERFORM UNTIL NO-REGALO

                   ADD WSC-20000 TO WSV-SALDOMOMENTO

                   IF WSV-SALDOMOMENTO >= WSV-ACCT-SALDO
                      OR WSV-CONTADORBOLETOS = 3
                      SET NO-REGALO TO TRUE
                   ELSE

                      IF WSV-SALDOMOMENTO <= WSV-ACCT-SALDO
                         SET REGALO-BOLETOS TO TRUE
                         PERFORM ESCRIBO-BOLETOS
                         ADD WSC-1 TO WSV-CONTADORBOLETOS
                      END-IF

                   END-IF

           END-PERFORM.



      ******************************************************
      * Parrafo para imprimir los encabezados del reporte  *
      ******************************************************
       ESCRIBO-ENCABEZADO.
           WRITE PRINT-REC FROM HEAD-1
           WRITE PRINT-REC FROM HEAD-VIR AFTER ADVANCING 1 LINES.
           WRITE PRINT-REC FROM BREAKLINE.
