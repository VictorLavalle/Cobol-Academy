      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.    CBLDB21
       AUTHOR.        Victor Lavalle.

      *________________________________________________________________*
      *                                                                *
      *                     "Practice 9"                               *
      *                                                                *
      * Instructions: Handle errors with SQLCODES, Commits and Rollback*
      *                                                                *
      *________________________________________________________________*


      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPOUT
                  ASSIGN TO UT-S-REPORT.


      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       FD  REPOUT
               RECORD CONTAINS 25 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS REPREC.

       01 REPREC.
          05 ACCT-STATE-O           PIC X(18).
          05 ACCT-CLIENTS-COUNT-O   PIC X(02).
          05 FILLER                 PIC X(05).


       WORKING-STORAGE SECTION.
       01 FLAGS.
          05 BANDERA-CURSOR         PIC X             VALUE SPACE.
             88 FIN-CURSOR                            VALUE 'Y'.
             88 NO-FIN-CURSOR                         VALUE 'N'.
      *****************************************************
                EXEC SQL INCLUDE SQLCA  END-EXEC.
      *****************************************************
       01 UD-ERROR-MESSAGE          PIC X(80)         VALUE SPACES.
       01 SQLCODES.
          05 SQLCODE0               PIC S9(9) COMP-5  VALUE 0.
          05 SQLCODE100             PIC S9(9) COMP-5  VALUE 100.

      *****************************************************
      * DECLARACION SQL DE LA TABLA                       *
      *****************************************************
                EXEC SQL DECLARE Z94474T TABLE
                        (ACCTNO CHAR(8) NOT NULL,
                         LIMIT DECIMAL(9,2) ,
                         BALANCE DECIMAL(9,2) ,
                         SURNAME CHAR(20) NOT NULL,
                         FIRSTN CHAR(15) NOT NULL,
                         ADDRESS1 CHAR(25) NOT NULL,
                         ADDRESS2 CHAR(20) NOT NULL,
                         ADDRESS3 CHAR(15) NOT NULL,
                         RESERVED CHAR(7) NOT NULL,
                         COMMENTS CHAR(50) NOT NULL)
                END-EXEC.
      *****************************************************
      * SQL CURSORS                                       *
      *****************************************************
                EXEC SQL DECLARE CURTABLA CURSOR FOR
                        SELECT * FROM Z94474T
                 END-EXEC.

      *    QUERY TO OBTAING THE NUMBER OF CLIENTS PER STATE
                 EXEC SQL DECLARE STATES CURSOR FOR
                         SELECT ADDRESS3, COUNT(*)
                         FROM Z94474T
                         GROUP BY ADDRESS3
                         ORDER BY 2 DESC
                  END-EXEC.

      *****************************************************
      * VARIABLES HOST EN DONDE RECIBIMOS LA TABLA        *
      *****************************************************
       01 VARIABLES-HOST.
          02 ACCT-STATE             PIC X(15).
          02 ACCT-CLIENTS-COUNT     PIC X(02).

       01 VARIABLES.
          05 WSV-PARG-NAME          PIC X(17).


       01 LOG-MESSAGES.
          05 WSV-LOG-MESSAGE        PIC X(80)         VALUE SPACES.
          05  QMLOG-FD              PIC S9(4) BINARY  VALUE 0.
          05  QMLOG-RECORD          PIC X(80)         VALUE SPACES.


       01 PARAGPH-NAMES.
          05 WSC-OPEN-CUR              PIC X(13) VALUE 'ABRIR CURSOR'.
          05 WSC-READ-CUR              PIC X(11) VALUE 'LEER CURSOR'.
          05 WSC-CLOSE-CUR             PIC X(13) VALUE 'CERRAR CURSOR'.
          05 WSC-EVAL-CODE             PIC X(17) VALUE
           'EVALUATE SQLCODES'.
          05 WSC-MAIN-PRO              PIC X(17) VALUE
           'PROCESO PRINCIPAL'.



       01 ERROR-MESSAGES.
          05 WSC-ERR-MSG-CUR        PIC X(28)         VALUE
                "Error en operacion de cursor".

          05 WSC-ERR-MSG-NOT-FOUND  PIC X(22)         VALUE
                "Registro no encontrado".

          05 WSC-ERR-MSG-EMPTY      PIC X(16)         VALUE
                "Registro vacio".

          05 WSC-ERR-MSG-PARG       PIC X(16)         VALUE
                "Error en parrafo".


       01 SUCCESS-MESSAGES.
          05 WSC-SUC-MSG-UPD        PIC X(21)         VALUE
                "Actualizacion exitosa".

          05 WSC-SUC-MSG-DEL        PIC X(19)         VALUE
                "Eliminacion exitosa".

          05 WSC-SUC-MSG-INS        PIC X(17)         VALUE
                "Insercion exitosa".

          05 WSC-SUC-MSG-COMMIT     PIC X(14)         VALUE
                "Commit exitoso".

          05 WSC-SUC-MSG-ROLLBACK   PIC X(16)         VALUE
                "Rollback exitoso".

          05 WSC-SUC-MSG-READ       PIC X(15)         VALUE
                "Lectura exitosa".

           05 WSC-SUC-MSG-CUR        PIC X(27)         VALUE
                "Operacion en cursor exitosa".

      *****************************************************
      * Print format for the report
      *****************************************************
       01 BREAKLINE.
          05 BLANK-SPACE            PIC X(50)         VALUE ALL " ".

       01 HEADER-1.
          05 FILLER                 PIC X(09)         VALUE 'Realizo: '
           .
          05 WSV-AUTHOR             PIC X(06).

       01 HEADER-2.
          05 FILLER                 PIC X(06)         VALUE
                'Estado'.
          05 FILLER                 PIC X(09)         VALUE SPACES.
          05 FILLER                 PIC X(06)         VALUE
                'Conteo'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       EMPIEZO-PROGRAMA.
           OPEN OUTPUT REPOUT.
           SET NO-FIN-CURSOR TO TRUE
           PERFORM PROCESO-PRINCIPAL.

       FIN-PROGRAMA.
           CLOSE REPOUT.
           GOBACK.


       PROCESO-PRINCIPAL.
           PERFORM ESCRIBE-ENCABEZADO
      *
           PERFORM ABRO-CURSOR
           PERFORM LEO-CURSOR

           IF SQLCODE = SQLCODE100
              MOVE WSC-MAIN-PRO TO WSV-PARG-NAME
              DISPLAY WSC-ERR-MSG-EMPTY ' en ' WSV-PARG-NAME ' SQLCODE:'
              SQLCODE

              STOP RUN
           END-IF
      *
           PERFORM CICLO-CURSOR UNTIL FIN-CURSOR
           PERFORM CIERRO-CURSOR.




       CICLO-CURSOR.
           PERFORM ESCRIBE-REPORTE.
           PERFORM LEO-CURSOR.



       ABRO-CURSOR.
           EXEC SQL
                OPEN STATES
           END-EXEC.

            MOVE WSC-OPEN-CUR TO WSV-PARG-NAME

           PERFORM QMLOG-OPEN-CURSOR.



       LEO-CURSOR.
           EXEC SQL
                FETCH STATES
                INTO :VARIABLES-HOST
           END-EXEC

           MOVE WSC-READ-CUR TO WSV-PARG-NAME

           PERFORM QMLOG-READ-CURSOR.



       CIERRO-CURSOR.
           EXEC SQL
                CLOSE STATES
           END-EXEC.

           MOVE WSC-OPEN-CUR TO WSV-PARG-NAME

           PERFORM QMLOG-CLOSE-CURSOR .


      ***************************************************
      * Paragraphs to print out the report of the data  *
      ***************************************************
       ESCRIBO-AUTOR.
           MOVE 'Victor' TO WSV-AUTHOR.
           WRITE REPREC FROM HEADER-1.


       ESCRIBE-REPORTE.
           INITIALIZE REPREC
           MOVE ACCT-STATE TO ACCT-STATE-O
           MOVE ACCT-CLIENTS-COUNT TO ACCT-CLIENTS-COUNT-O
           WRITE REPREC AFTER ADVANCING 1 LINES.

       ESCRIBE-ENCABEZADO.
           PERFORM ESCRIBO-AUTOR
           WRITE REPREC FROM BREAKLINE AFTER ADVANCING 1 LINES
           WRITE REPREC FROM HEADER-2
           WRITE REPREC FROM BREAKLINE.


      ****************************************************
      * The EVALUATE-SQLCODES subroutine evaluates the   *
      * SQLCODE value and sets flags accordingly.        *
      ****************************************************
       EVALUATE-SQLCODES.

           EVALUATE SQLCODE
               WHEN SQLCODE0
                    SET NO-FIN-CURSOR TO TRUE
               WHEN SQLCODE100
                    SET FIN-CURSOR TO TRUE
                    MOVE WSC-EVAL-CODE TO WSV-PARG-NAME
                    DISPLAY WSC-ERR-MSG-EMPTY ' al ejecutar '
                    WSV-PARG-NAME ' - SQLCODE:' SQLCODE
               WHEN OTHER
                    DISPLAY WSC-SUC-MSG-ROLLBACK ' al ejecutar '
                    WSV-PARG-NAME ' - SQLCODE:' SQLCODE
                    STOP RUN
           END-EVALUATE.


      ****************************************************
      * Functions to print a log of the program running  *
      ****************************************************
       QMLOG-ERROR.
           DISPLAY '******************************************'.
           DISPLAY '*         OOPS, ALGO SALIO MAL:          *'
           DISPLAY '*' WSC-ERR-MSG-CUR ' EN ' WSV-PARG-NAME '*'
           DISPLAY '* SQLCODE: ' SQLCODE'                    *'
           DISPLAY '******************************************'.

       QMLOG-READ-CURSOR.
           EVALUATE SQLCODE
               WHEN SQLCODE0
                    DISPLAY WSC-SUC-MSG-CUR ' en ' WSV-PARG-NAME
               WHEN OTHER
                    PERFORM QMLOG-ERROR
                    PERFORM EVALUATE-SQLCODES
           END-EVALUATE.



       QMLOG-OPEN-CURSOR.
           EVALUATE SQLCODE
               WHEN SQLCODE0
                    DISPLAY WSC-SUC-MSG-CUR ' en ' WSV-PARG-NAME
               WHEN OTHER
                    PERFORM QMLOG-ERROR
                    PERFORM EVALUATE-SQLCODES
           END-EVALUATE.



       QMLOG-CLOSE-CURSOR.
           EVALUATE SQLCODE
               WHEN SQLCODE0
                    DISPLAY WSC-SUC-MSG-CUR ' en ' WSV-PARG-NAME
               WHEN OTHER
                    PERFORM QMLOG-ERROR
                    PERFORM EVALUATE-SQLCODES
           END-EVALUATE.





