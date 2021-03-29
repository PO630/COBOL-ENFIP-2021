      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3-DEPTS.
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT F-COMMUNES ASSIGN TO "3-communes.dat"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT FS-DEPTS ASSIGN TO "4-dept.dat"
               ORGANIZATION LINE SEQUENTIAL.

      ******************************************************************

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

      ******************************************************************

       WORKING-STORAGE SECTION.

       77  FIN-ENREG PIC X VALUE SPACE.
           88 FF VALUE HIGH-VALUE.
       77  FIN-ECRIT PIC X VALUE SPACE.
           88 FE VALUE HIGH-VALUE.

       01  WSS-DATA.
           05 IND PIC 9(03).
           05 CODE-INSEE.
               10 CODE-TEMP PIC 9(3).
                   88 CODE-OUTREMER-SIMPLE     VALUE 971 THRU 974.
                   88 CODE-OUTREMER-COMPLEXE   VALUE 976.
               10 CODE-TEMP-COMPLEXE REDEFINES CODE-TEMP.
                   15 CODE-DEPT PIC 99.
                   15 CODE-OUTREMER PIC 9.

               10 RESTE PIC 99.

       77  CODE-DEPT-CALCULE PIC 999.
       77  CPT PIC 999 VALUE 1.
       77  NOM-COMMUNE PIC X(50).
       77  NOM-DEPT PIC X(30).
       77  NOM-REGION PIC X(30).
       01  TABLEAU-DEPT.
           05 DEPT OCCURS 101.
               10 NB-COMMUNES PIC 9(3) VALUE 0.
               10 ALTITUDE-DEPT PIC 9(6)V99.
               10 SUPERFICIE-DEPT PIC 9(6)V99.
               10 POPULATION-DEPT PIC 9(6)V99.
               10 TAUX-CARBONE PIC 9(4)V99.
               10 TAUX-DENSITE PIC 9(4)V99.

      ******************************************************************

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           PERFORM READ-ALL-COMMUNES
           PERFORM CALCUL-TAUX
           PERFORM WRITE-DEPT
           PERFORM FIN-PGM

           .

      ******************************************************************


       FIND-NUMERO-DEPARTEMENT.
      *    Trouve le numero du departement concerne
      *    Return : CODE-DEPT-CALCULE
           INITIALIZE CODE-DEPT-CALCULE
           EVALUATE TRUE
               WHEN CODE-OUTREMER-SIMPLE
                   COMPUTE CODE-DEPT-CALCULE =
                           CODE-DEPT + CODE-OUTREMER - 1
               WHEN CODE-OUTREMER-COMPLEXE
                   MOVE 101 TO CODE-DEPT-CALCULE
               WHEN OTHER
                   MOVE CODE-DEPT TO CODE-DEPT-CALCULE
               END-EVALUATE
           .

       READ-ALL-COMMUNES.

           OPEN INPUT F-COMMUNES

           PERFORM UNTIL FF

                READ F-COMMUNES
                   AT END
                       SET FF TO TRUE
                   NOT AT END
                       MOVE C-COMMUNE TO NOM-COMMUNE
                       MOVE C-CODE-INSEE TO CODE-INSEE

                       PERFORM FIND-NUMERO-DEPARTEMENT
                           
                       ADD 1 TO NB-COMMUNES(CODE-DEPT-CALCULE)
                           
                       COMPUTE
                                   ALTITUDE-DEPT(CODE-DEPT-CALCULE) =
                                   ALTITUDE-DEPT(CODE-DEPT-CALCULE)
                                   + C-ALTITUDE
                       COMPUTE
                                   SUPERFICIE-DEPT(CODE-DEPT-CALCULE) =
                                   SUPERFICIE-DEPT(CODE-DEPT-CALCULE)
                                   + C-SUPERFICIE
                       COMPUTE
                                   POPULATION-DEPT(CODE-DEPT-CALCULE) =
                                   POPULATION-DEPT(CODE-DEPT-CALCULE)
                                   + C-POPULATION

           END-PERFORM

           CLOSE F-COMMUNES
           .

       CALCUL-TAUX.

           PERFORM UNTIL CPT > 101

               COMPUTE ALTITUDE-DEPT(CPT) =
                   ALTITUDE-DEPT(CPT) / NB-COMMUNES(CPT)

               COMPUTE POPULATION-DEPT(CPT) =
                   POPULATION-DEPT(CPT) / NB-COMMUNES(CPT)

               COMPUTE SUPERFICIE-DEPT(CPT) =
                   SUPERFICIE-DEPT(CPT) / NB-COMMUNES(CPT)

               COMPUTE TAUX-CARBONE(CPT) =
                   (3000 - ALTITUDE-DEPT(CPT)) * 0.0005

               COMPUTE TAUX-DENSITE(CPT) =
                   1 +
                   (POPULATION-DEPT(CPT)*1000/SUPERFICIE-DEPT(CPT))/100

               ADD 1 TO CPT

           END-PERFORM

           .


       WRITE-DEPT.

           OPEN INPUT F-COMMUNES
           OPEN OUTPUT FS-DEPTS

           PERFORM UNTIL FE

               READ F-COMMUNES

                   AT END
                       SET FE TO TRUE

                   NOT AT END

                       MOVE C-CODE-INSEE TO D-CODE-INSEE CODE-INSEE
                       MOVE C-LIEU TO D-LIEU

                       PERFORM FIND-NUMERO-DEPARTEMENT

                       MOVE ALTITUDE-DEPT(CODE-DEPT-CALCULE) TO
                            D-ALTITUDE
                       MOVE SUPERFICIE-DEPT(CODE-DEPT-CALCULE) TO
                            D-SUPERFICIE
                       MOVE POPULATION-DEPT(CODE-DEPT-CALCULE) TO
                            D-POPULATION
                       MOVE TAUX-CARBONE(CODE-DEPT-CALCULE) TO
                            D-TAUX-CARBONE
                       MOVE TAUX-DENSITE(CODE-DEPT-CALCULE) TO
                            D-TAUX-DENSITE
                       WRITE F-ART-DEPT

           END-PERFORM

           CLOSE F-COMMUNES FS-DEPTS
           .



      ******************************************************************
       FIN-PGM.
            STOP RUN.
      ******************************************************************
