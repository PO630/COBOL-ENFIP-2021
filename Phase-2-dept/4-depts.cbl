      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT F-COMMUNES ASSIGN TO "3-communes.dat"
           ORGANIZATION LINE SEQUENTIAL.
       SELECT FS-DEPTS ASSIGN TO "4-dept.dat"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  F-COMMUNES.
           01 F-ART-COM.
              05 C-CODE-INSEE PIC 9(5).
              05 C-LIEU.
                  10 C-COMMUNE PIC X(50).
                  10 C-DEPARTEMENT PIC X(28).
                  10 C-REGION PIC X(30).
              05 C-DONNEES.
                  10 C-ALTITUDE PIC 9(6)V99.
                  10 C-SUPERFICIE PIC 9(6)V99.
                  10 C-POPULATION PIC 9(6)V99.
              05 C-TAUX.
                  10 C-TAUX-CARBONE PIC 9(4)V99.
                  10 C-TAUX-DENSITE PIC 9(4)V99.

       FD  FS-DEPTS.
           01 F-ART-DEPT.
              05 D-CODE-INSEE PIC 9(5).
              05 D-LIEU.
                  10 D-COMMUNE PIC X(50).
                  10 D-DEPARTEMENT PIC X(28).
                  10 D-REGION PIC X(30).
              05 D-DONNEES.
                  10 D-ALTITUDE PIC 9(6)V99.
                  10 D-SUPERFICIE PIC 9(6)V99.
                  10 D-POPULATION PIC 9(6)V99.
              05 D-TAUX.
                  10 D-TAUX-CARBONE PIC 9(4)V99.
                  10 D-TAUX-DENSITE PIC 9(4)V99.


       WORKING-STORAGE SECTION.
       77  FIN-ENREG PIC X VALUE SPACE.
           88 FF VALUE HIGH-VALUE.



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT F-COMMUNES
            OPEN OUTPUT FS-DEPTS

            PERFORM UNTIL FF

                READ F-COMMUNES
                   AT END
                       SET FF TO TRUE
                   NOT AT END
                       DISPLAY F-ART-COM

      ******************** CALCUL TAUX CARBONE *************************



            END-PERFORM

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
