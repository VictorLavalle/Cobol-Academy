      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBL0001
       AUTHOR.         Victor Lavalle.
       DATE-WRITTEN    FEBRERO 2022.

      *________________________________________________________________*
      *                                                                *
      *                     "PRACTICA 4"                               *
      *                                                                *
      * Instrucciones: Hacer un programa Cobol que genere un archivo   *
      *          con ID de cliente, Nombre del cliente, Saldo en Euros *
      *          y Ciudad del cliente                                  *
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
          05 WSV-ACCT-ID-S     PIC X(10).
          05 WSV-NOMBRE-S      PIC X(15).
          05 WSV-ACCT-SALDO-S  PIC $$,$$$,$$9.99.
          05 WSV-DIRECCION-S.
             10 WSV-CIUDAD-S   PIC X(20).


      *Este es el archivo de entrada
       FD  ACCT-REC RECORDING MODE F.
       01 ACCT-FIELDS.
           05  WSV-ACCT-ID            PIC X(8).
           05  WSV-ACCT-LIMITE        PIC S9(7)V99 COMP-3.
           05  WSV-ACCT-SALDO         PIC S9(7)V99 COMP-3.
           05  WSV-APELLIDO-P         PIC X(20).
           05  WSV-NOMBRE             PIC X(15).
           05  WSV-DIRECCION.
               10  WSV-CALLE          PIC X(25).
               10  WSV-CIUDAD         PIC X(20).
               10  WSV-ESTADO         PIC X(15).
           05  WSV-RESERVED           PIC X(7).
           05  WSV-COMENTARIOS        PIC X(50).



       WORKING-STORAGE SECTION.
       01 FLAGS.
          05 BANDERA-FILE      PIC X             VALUE SPACE.
             88 FIN-FILE                         VALUE 'Y'.
             88 NO-FIN-FILE                      VALUE 'N'.

       01 CONSTANTES.
          05 WSC-EUR-DOLAR     PIC 9(02)V99      VALUE 1.06.
          05 WSC-CONTADOR      PIC 9(2)          VALUE 1.
          05 WSC-DIVISOR       PIC 9(2)          VALUE 2.
          05 WSC-LIMITE        PIC 9(2)          VALUE 21.
          05 WSC-MODULO-PAR    PIC 9(2)          VALUE 0.


       01 VARIABLES.
          05 WSV-MODULO        PIC 9(2).
          05 WSV-RESULT        PIC 9(2).



      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       ABRO-ARCHIVOS.
           OPEN INPUT ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
      *
       PROCESO-PRINCIPAL.
           SET NO-FIN-FILE TO TRUE
           PERFORM LEE-ARCHIVO
           PERFORM UNTIL FIN-FILE
      *
      *
      * Se pregunta si el contador es menor al limite
      * para que solo traiga 20 registros
      *
            IF WSC-CONTADOR < WSC-LIMITE
      *         COMPUTE WSV-RESULT = WSC-CONTADOR / WSC-DIVISOR
               DIVIDE WSC-CONTADOR BY WSC-DIVISOR
                    GIVING WSV-RESULT REMAINDER WSV-MODULO
      *
      * Se pregunta si el residuo es 0 para savber si es par.s
      * Si es par se escribe el registro
               IF WSV-MODULO = WSC-MODULO-PAR
                 COMPUTE WSV-ACCT-SALDO = WSV-ACCT-SALDO *
                            WSC-EUR-DOLAR
                  PERFORM ESCRIBO-ARCHIVO
               END-IF

            END-IF
      *
      * Se aumenta el contador para que no se quede en un bucle infinito
      * y se vaya leyendo el archivo
      *
            ADD 1 TO WSC-CONTADOR
            PERFORM LEE-ARCHIVO

           END-PERFORM.
      *
       CIERRO-Y-ACABO.
           CLOSE ACCT-REC.
           CLOSE PRINT-LINE.
           GOBACK.
      *
       LEE-ARCHIVO.
           READ ACCT-REC
           AT END
              SET FIN-FILE TO TRUE
           END-READ.
      *
       ESCRIBO-ARCHIVO.
           MOVE WSV-ACCT-ID TO WSV-ACCT-ID-S.
           MOVE WSV-NOMBRE TO WSV-NOMBRE-S.
           MOVE WSV-ACCT-SALDO TO WSV-ACCT-SALDO-S.
           MOVE WSV-CIUDAD TO WSV-CIUDAD-S.

           WRITE PRINT-REC.
      *
