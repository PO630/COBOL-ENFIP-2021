      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3-REGIONS.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT F-COMMUNES ASSIGN TO "3-communes.dat"
                   ORGANIZATION LINE SEQUENTIAL.

           SELECT C-REGIONS ASSIGN TO "4-regions.dat"
                   ORGANIZATION LINE SEQUENTIAL.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  F-COMMUNES.
       01  F-COMMUNES-DATA.
           05 E-Code-Insee  PIC 9(5).

           05 E-Lieu.
               10 E-Commune PIC X(50).
               10 E-Departement PIC X(28).
               10 E-Region PIC X(30).

           05 E-Donnees.
               10 E-Altitude         PIC 9(6)V99.
               10 E-Superficie       PIC 9(6)V99.
               10 E-Population       PIC 9(6)V99.

           05 E-Taux.
               10 E-Taux-Carbone PIC 9(4)V99.
               10 E-Taux-Densite PIC 9(4)V99.


       FD  C-REGIONS.
       01  C-REGIONS-DATA.
           05 C-Code-Insee  PIC 9(5).
           05 C-Lieu.
               10 C-Commune PIC X(50).
               10 C-Departement PIC X(28).
               10 C-Region PIC X(30).
           05 C-Donnees.
               10 C-Altitude            PIC 9(6)V99.
               10 C-Superficie          PIC 9(6)V99.
               10 C-Population          PIC 9(6)V99.
           05 C-Taux.
               10 C-Taux-Carbone        PIC 9(4)V99.
               10 C-Taux-Densite        PIC 9(4)V99.


      ******************************************************************
       WORKING-STORAGE SECTION.

       1   DATA-WORKING-MANAGER.

           5 TABLE-INDEX     PIC 9(2)      VALUE 1.
               88  TABLE-INDEX-END         VALUE 19.
           5 NOMBRE-REGION   PIC 9(2)      VALUE 18.

           5 TABLE-REGION    OCCURS 18.
      *        INDEXED KEY
               10  NOM-REGION          PIC X(30).
               10  INDEXED-COMPLETE    PIC 9(1)    VALUE 0.
                   88  INDEXED-COMPLETE-TRUE       VALUE 1.
                   88  INDEXED-COMPLETE-FALSE      VALUE 0.
      *        VALUE
               10  NOMBRE-COMMUNES     PIC 9(6)    VALUE 0.
               10  SOMME-ALTITUDE      PIC 9(6)V99 VALUE 0.
               10  SOMME-SUPERFICIE    PIC 9(6)V99 VALUE 0.
               10  SOMME-POPULATION    PIC 9(6)V99 VALUE 0.
      *        TAUX
               10 TAUX-CARBONE PIC 9(4)V99         VALUE 0.
               10 TAUX-DENSITE PIC 9(4)V99         VALUE 0.



       1   FILE-WORKING-MANAGER.
      * ++===                                fin article rencontre ===++
           05  FIN-ENREG                PIC  X(01) VALUE  SPACE.
               88  FF                              VALUE  HIGH-VALUE.

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT F-COMMUNES

           PERFORM UNTIL FF
               READ F-COMMUNES
                   AT END
                       SET FF TO TRUE
                   NOT AT END
                       PERFORM COMPLETE-TABLE
               END-READ
           END-PERFORM

           PERFORM DISPLAY-TABLE

           PERFORM COMPUTE-TABLE-VALUE
           PERFORM WRITE-REGIONS


           CLOSE F-COMMUNES
           PERFORM FIN-PGM
           .
      ******************************************************************
      *        FONCTION FX

       COMPLETE-TABLE.

           MOVE 1 TO TABLE-INDEX

           PERFORM UNTIL TABLE-INDEX-END

               IF NOM-REGION(TABLE-INDEX) = E-Region THEN

      *            ADD VALUE TO OCCUR
                   ADD 1 TO NOMBRE-COMMUNES(TABLE-INDEX)

                   COMPUTE SOMME-ALTITUDE(TABLE-INDEX) =
                           SOMME-ALTITUDE(TABLE-INDEX) + E-Altitude

                   COMPUTE SOMME-SUPERFICIE(TABLE-INDEX) =
                           SOMME-SUPERFICIE(TABLE-INDEX) + E-Superficie

                   COMPUTE SOMME-SUPERFICIE(TABLE-INDEX) =
                           SOMME-SUPERFICIE(TABLE-INDEX) + E-Population

      *            END PERFORM => 18
                   MOVE NOMBRE-REGION TO TABLE-INDEX

               ELSE

                   IF INDEXED-COMPLETE-FALSE(TABLE-INDEX) THEN

      *            INDEXED OCCURS
                   MOVE E-Region TO NOM-REGION(TABLE-INDEX)
                   MOVE 1 TO INDEXED-COMPLETE(TABLE-INDEX)

      *            ADD VALUE TO OCCUR
                   ADD 1 TO NOMBRE-COMMUNES(TABLE-INDEX)

                   COMPUTE SOMME-ALTITUDE(TABLE-INDEX) =
                           SOMME-ALTITUDE(TABLE-INDEX) + E-Altitude

                   COMPUTE SOMME-SUPERFICIE(TABLE-INDEX) =
                           SOMME-SUPERFICIE(TABLE-INDEX) + E-Superficie

                   COMPUTE SOMME-SUPERFICIE(TABLE-INDEX) =
                           SOMME-SUPERFICIE(TABLE-INDEX) + E-Population

      *            END PERFORM => 18
                   MOVE NOMBRE-REGION TO TABLE-INDEX
                   END-IF

               END-IF
               ADD 1 TO TABLE-INDEX
           END-PERFORM
           .

       DISPLAY-TABLE.

           MOVE 1 TO TABLE-INDEX

           PERFORM UNTIL TABLE-INDEX-END

               DISPLAY NOM-REGION(TABLE-INDEX)
                           " "NOMBRE-COMMUNES(TABLE-INDEX)
               ADD 1 TO TABLE-INDEX

           END-PERFORM
           .

      ******************************************************************
      *        FONCTION JONAS

       COMPUTE-TABLE-VALUE.

           MOVE 1 TO TABLE-INDEX

           PERFORM UNTIL TABLE-INDEX-END

      *        COMPUTE TAUX-CARBONE(TABLE-INDEX) = 
               
      *        COMPUTE TAUX-DENSITE(TABLE-INDEX) = 

               ADD 1 TO TABLE-INDEX

           END-PERFORM
           .

       WRITE-REGIONS.

           MOVE SPACE TO FIN-ENREG
           OPEN INPUT F-COMMUNES
           OPEN OUTPUT C-REGIONS

           PERFORM UNTIL FF
               READ F-COMMUNES
                   AT END
                       SET FF TO TRUE
                   NOT AT END
                       
                   MOVE E-Code-Insee   TO C-Code-Insee
                   MOVE E-Lieu         TO C-Lieu
                   MOVE E-Donnees      TO C-Donnees

      *            SEARCH TAUX IN TABLE
                   PERFORM SEARCH-TAUX-TABLE

               END-READ
           END-PERFORM
           CLOSE F-COMMUNES C-REGIONS
           .

       SEARCH-TAUX-TABLE.

           MOVE 1 TO TABLE-INDEX

           PERFORM UNTIL TABLE-INDEX-END

               IF NOM-REGION(TABLE-INDEX) = E-Region THEN
               
      *        MOVE TAUX IN C-TAUX ...
      *        ?

      *        BREAK PERFOM WITH 18
               MOVE NOMBRE-REGION TO TABLE-INDEX
               END-IF

               ADD 1 TO TABLE-INDEX

           END-PERFORM
           .


      ******************************************************************
       FIN-PGM.
           STOP RUN
           .
      ******************************************************************
