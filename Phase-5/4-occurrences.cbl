      ******************************************************************
      * Author:    Marcheix François-Xavier
      * Date:      23/03/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    Main file
           SELECT  F-OCCURRENCES ASSIGN TO "4-occurrences.dat"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT  C-OCCURRENCES ASSIGN TO "5-occurrences.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS C-PRIMARY-KEY.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  F-OCCURRENCES.
       01  F-OCCURRENCES-ENREG.
           05 F-NUMERO-FISCAL  PIC 9(8).
           05 F-VALUE.
               10 F-TAXE       PIC X(2).
               10 F-REVENU     PIC 9(6).
               10 F-CODE-INSEE PIC 9(5).


       FD  C-OCCURRENCES.
       01  C-OCCURRENCES-ENREG.
      *    Primary Key
           05 C-PRIMARY-KEY            PIC 9(4).
      *    Value
           05 C-NUMERO-FISCAL   PIC 9(8).
           05 C-VALUE.
               10 C-TAXE        PIC X(2).
               10 C-REVENU      PIC 9(6).
               10 C-CODE-INSEE  PIC 9(5).

      ******************************************************************
       WORKING-STORAGE SECTION.

       1   WORKING-DATA-MANAGER.

           05 NEW-PRIMARY-KEY           PIC 9(4) VALUE 1.

       1   FILE-WORKING-MANAGER.
      * ++===                                fin article rencontre ===++
           05  FIN-ENREG                PIC  X(01) VALUE  SPACE.
               88  FF                              VALUE  HIGH-VALUE.

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT F-OCCURRENCES
           OPEN OUTPUT C-OCCURRENCES

           PERFORM UNTIL FF

              READ F-OCCURRENCES
                AT END
                   SET FF TO TRUE
                NOT AT END


                   MOVE NEW-PRIMARY-KEY TO C-PRIMARY-KEY
                   MOVE F-VALUE TO C-VALUE

                   WRITE C-OCCURRENCES-ENREG
                       INVALID KEY
                           DISPLAY NEW-PRIMARY-KEY" IS USED"
                       NOT INVALID KEY
                           DISPLAY C-OCCURRENCES-ENREG
                   END-WRITE

                   ADD 1 TO NEW-PRIMARY-KEY

              END-READ
           END-PERFORM

           CLOSE F-OCCURRENCES C-OCCURRENCES

           .

       FIN-PGM.
           STOP RUN.
