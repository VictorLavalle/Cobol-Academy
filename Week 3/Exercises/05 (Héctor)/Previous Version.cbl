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
          05 WSV-ACCT-ID-S      PIC X(10).
          05 WSV-NOMBRE-S       PIC X(15).
          05 FILLER             PIC X(03)         VALUE SPACES.
          05 WSAV-APELLIDO-P-S  PIC X(20).
          05 WSV-ACCT-SALDO-S   PIC $$,$$$,$$9.99.
          05 WSV-DIRECCION-S.
             10 WSV-CIUDAD-S    PIC X(20).


      *Este es el archivo de entrada
       FD  ACCT-REC RECORDING MODE F.
       01 ACCT-FIELDS.
          05 WSV-ACCT-ID        PIC X(8).
          05 WSV-ACCT-LIMITE    PIC S9(7)V99 COMP-3.
          05 WSV-ACCT-SALDO     PIC S9(7)V99 COMP-3.
          05 WSV-APELLIDO-P     PIC X(20).
          05 WSV-NOMBRE         PIC X(15).
          05 WSV-DIRECCION.
             10 WSV-CALLE       PIC X(25).
             10 WSV-CIUDAD      PIC X(20).
             10 WSV-ESTADO      PIC X(15).
          05 WSV-RESERVED       PIC X(7).
          05 WSV-COMENTARIOS    PIC X(50).


       WORKING-STORAGE SECTION.
       01 FLAGS.
          05 BANDERA-FILE       PIC X             VALUE SPACE.
             88 FIN-FILE                          VALUE 'Y'.
             88 NO-FIN-FILE                       VALUE 'N'.

       01 CONSTANTES.
          05 WSC-EUR-DOLAR      PIC 9(02)V99      VALUE 1.06.
          05 WSC-CONTADOR       PIC 9(2)          VALUE 1.
          05 WSC-DIVISOR        PIC 9(2)          VALUE 2.
          05 WSC-LIMITE         PIC 9(2)          VALUE 21.
          05 WSC-MODULO-PAR     PIC 9(2)          VALUE 0.


       01 VARIABLES.
          05 WSV-MODULO         PIC 9(2).
          05 WSV-RESULT         PIC 9(2).

       01 WS-CURRENT-DATE-DATA.
          05 WS-CURRENT-DATE.
             10 WS-ANIO         PIC 9(02).
             10 WS-MES          PIC 9(02).
             10 WS-DIA          PIC 9(02).
          05 WS-CURRENT-TIME.
             10 WS-HORA         PIC 9(02).
             10 WS-MINUTO       PIC 9(02).
             10 WS-SEGUNDO      PIC 9(02).


       01 WS-CURRENT-DATE.
          05 WS-DATE.
             10 WS-DATE-ANIO    PIC 9(02).
             10 WS-DATE-MES     PIC 9(02).
             10 WS-DATE-DIA     PIC 9(02).


       01 ENCA-1.
          05 FILLER             PIC X(20)         VALUE
                'Reporte Financiero'.
          05 FILLER             PIC X(22)         VALUE
                'Elabora: Víctor       '.
          05 FILLER             PIC X(60)         VALUE SPACES.

       01 ENCA-2.
          05 FILLER             PIC X(15)         VALUE
                'Nombre Cliente '.
          05 FILLER             PIC X(03)         VALUE SPACES.
          05 FILLER             PIC X(20)         VALUE
                'Apellido Paterno    '.

       01 ENCA-3.
          05 FILLER             PIC X(15)         VALUE
                '---------------'.
          05 FILLER             PIC X(03)         VALUE SPACES.
          05 FILLER             PIC X(20)         VALUE '-------------'.

       01 BREAKLINE.
          05 BLANK-SPACE        PIC X(80)         VALUE ALL " ".

       01 FOOTER.
          05 FILLER             PIC X(14)         VALUE 'Fecha y Hora: '
           .
          05 FILLER             PIC X(01)         VALUE SPACES.
          05 WS-HORA-S          PIC X(02).
          05 FILLER             PIC X             VALUE ':'.
          05 WS-MINUTO-S        PIC 9(02).
          05 FILLER             PIC X             VALUE ':'.
          05 WS-SEGUNDO-S       PIC 9(02).
          05 FILLER             PIC X(01)         VALUE SPACES.
          05 WS-AM-PM           PIC XX            VALUE SPACES.
          05 FILLER             PIC X             VALUE SPACES.
          05 FILLER             PIC X             VALUE '-'.
          05 FILLER             PIC X             VALUE SPACES.
          05 WS-DIA-S           PIC 9(02).
          05 FILLER             PIC X             VALUE '/'.
          05 WS-MES-S           PIC 9(02).
          05 FILLER             PIC X             VALUE '/'.
          05 WS-ANIO-S          PIC 9(02).


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       ABRO-ARCHIVOS.
           OPEN INPUT ACCT-REC.
           OPEN OUTPUT PRINT-LINE.
      *
       PROCESO-PRINCIPAL.
           SET NO-FIN-FILE TO TRUE

      *
      * Se escribe el encabezado del reporte
      *
           PERFORM ESCRIBO-ENCABEZADO
           PERFORM LEE-ARCHIVO

           PERFORM UNTIL FIN-FILE
      *
      * Se pregunta si el contador es menor al limite
      * para que solo traiga 20 registros
      *
                   IF WSC-CONTADOR < WSC-LIMITE
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

           END-PERFORM

           PERFORM IMPRIME-FECHA.

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
      *     MOVE WSV-ACCT-ID TO WSV-ACCT-ID-S.
           MOVE WSV-NOMBRE TO WSV-NOMBRE-S.
           MOVE WSV-APELLIDO-P TO WSAV-APELLIDO-P-S.
      *     MOVE WSV-ACCT-SALDO TO WSV-ACCT-SALDO-S.
      *     MOVE WSV-CIUDAD TO WSV-CIUDAD-S.

           WRITE PRINT-REC.
      *
       ESCRIBO-ENCABEZADO.
           WRITE PRINT-REC FROM ENCA-1
           WRITE PRINT-REC FROM ENCA-2
           WRITE PRINT-REC FROM ENCA-3
           WRITE PRINT-REC FROM BREAKLINE.

       IMPRIME-FECHA.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           ACCEPT WS-DATE FROM DATE
           ACCEPT WS-CURRENT-TIME FROM TIME

           MOVE WS-DATE-ANIO TO WS-ANIO-S
           MOVE WS-DATE-MES TO WS-MES-S
           MOVE WS-DATE-DIA TO WS-DIA-S

           MOVE WS-HORA TO WS-HORA-S
           MOVE WS-MINUTO TO WS-MINUTO-S
           MOVE WS-SEGUNDO TO WS-SEGUNDO-S

           IF WS-HORA >= 12
              ADD -12 TO WS-HORA
              MOVE "PM" TO WS-AM-PM
           ELSE
              MOVE "AM" TO WS-AM-PM
           END-IF

           WRITE PRINT-REC FROM BREAKLINE
           WRITE PRINT-REC FROM FOOTER.

