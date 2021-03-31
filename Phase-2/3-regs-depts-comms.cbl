      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3-REGS-DEPTS-COMMS.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT F-COMMUNES   ASSIGN TO "3-communes.dat"
                   ORGANIZATION LINE SEQUENTIAL.

      *    Phase-2-file
           SELECT C-COMMUNES   ASSIGN TO "5-communes.idx"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS C-Code-Insee.

           SELECT C-DEPTS      ASSIGN TO "5-depts.idx"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS D-Departement.

           SELECT C-REGIONS    ASSIGN TO "5-regions.idx"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS RANDOM
                   RECORD KEY IS R-Region.

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


      *    Phase-2-file
       FD  C-COMMUNES.
       01  C-COM-ENREG.
           05 C-Code-Insee          PIC 9(5).
           05 C-Lieu.
               10 C-Commune             PIC X(50).
               10 C-Departement         PIC X(28).
               10 C-Region              PIC X(30).
           05 C-Donnees.
               10 C-Altitude            PIC 9(6)V99.
               10 C-Superficie          PIC 9(6)V99.
               10 C-Population          PIC 9(6)V99.
           05 C-Taux.
               10 C-Taux-Carbone        PIC 9(4)V99.
               10 C-Taux-Densite        PIC 9(4)V99.

       FD  C-DEPTS.
       01  C-DEP-ENREG.
           05 D-Lieu.
               10 D-Communes            PIC 9(6).
               10 D-Departement         PIC X(28).
               10 D-Region              PIC X(30).
           05 D-Donnees.
               10 D-Altitude            PIC 9(6)V99.
               10 D-Superficie          PIC 9(6)V99.
               10 D-Population          PIC 9(6)V99.
           05 D-Taux.
               10 D-Taux-Carbone        PIC 9(4)V99.
               10 D-Taux-Densite        PIC 9(4)V99.


       FD  C-REGIONS.
       01  C-REG-ENREG.
           05 R-Lieu.
               10 R-Communes            PIC 9(6).
               10 R-Region              PIC X(30).
           05 R-Donnees.
               10 R-Altitude            PIC 9(6)V99.
               10 R-Superficie          PIC 9(6)V99.
               10 R-Population          PIC 9(6)V99.
           05 R-Taux.
               10 R-Taux-Carbone        PIC 9(4)V99.
               10 R-Taux-Densite        PIC 9(4)V99.

      ******************************************************************
       WORKING-STORAGE SECTION.

       1   DATA-WORKING-MANAGER.

           5 TABLE-INDEX     PIC 9(3)      VALUE 1.
               88  TABLE-INDEX-END-R           VALUE 28.
               88  TABLE-INDEX-END-D           VALUE 102.

           5 NOMBRE-REGION     PIC 9(2)        VALUE 27.
           5 NOMBRE-DEPTS      PIC 9(3)        VALUE 101.

           5 TABLE-REGION    OCCURS 27.
      *        INDEXED KEY
               10  R-INDEXED-COMPLETE    PIC 9(1)    VALUE 0.
                   88  R-INDEXED-COMPLETE-TRUE       VALUE 1.
                   88  R-INDEXED-COMPLETE-FALSE      VALUE 0.
               10  R-TAB-LIEU.
                   15  R-NOMBRE-COMMUNES       PIC 9(6)    VALUE 0.
                   15  R-NOM                   PIC X(30).
      *        VALUE
               10  R-TAB-DONNEES.
                   15  R-SOMME-ALTITUDE      PIC 9(6)V99 VALUE 0.
                   15  R-SOMME-SUPERFICIE    PIC 9(6)V99 VALUE 0.
                   15  R-SOMME-POPULATION    PIC 9(6)V99 VALUE 0.
      *        TAUX
               10 R-TAB-TAUX.
                   15 R-TAB-TAUX-CARBONE PIC 9(4)V99     VALUE 0.
                   15 R-TAB-TAUX-DENSITE PIC 9(4)V99     VALUE 0.


           5 TABLE-DEPTS    OCCURS 101.
               10  D-INDEXED-COMPLETE    PIC 9(1)    VALUE 0.
                   88  D-INDEXED-COMPLETE-TRUE       VALUE 1.
                   88  D-INDEXED-COMPLETE-FALSE      VALUE 0.

               10 D-TABLE-LIEU.
                   15  D-NOMBRE-COMMUNES     PIC 9(6)    VALUE 0.
      *            INDEXED KEY
                   15  D-NOM                         PIC X(28).
                   15  D-TABLE-REGION                PIC X(30).
               10 D-TABLE-DONNEES.
                   15  D-SOMME-ALTITUDE      PIC 9(6)V99 VALUE 0.
                   15  D-SOMME-SUPERFICIE    PIC 9(6)V99 VALUE 0.
                   15  D-SOMME-POPULATION    PIC 9(6)V99 VALUE 0.
               10 D-TABLE-TAUX.
                   15 D-TABLE-TAUX-CARBONE PIC 9(4)V99         VALUE 0.
                   15 D-TABLE-TAUX-DENSITE PIC 9(4)V99         VALUE 0.



       1   FILE-WORKING-MANAGER.
      * ++===                                fin article rencontre ===++
           05  FIN-ENREG                PIC  X(01) VALUE  SPACE.
               88  FF                              VALUE  HIGH-VALUE.

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT F-COMMUNES
           OPEN OUTPUT C-DEPTS C-COMMUNES C-REGIONS

           PERFORM UNTIL FF
               READ F-COMMUNES
                   AT END
                       SET FF TO TRUE
                   NOT AT END

                       PERFORM COMPLETE-TABLE-REGION
                       PERFORM COMPLETE-TABLE-DEPT
                       PERFORM WRITE-COMMUNES-IDX

               END-READ
           END-PERFORM

           PERFORM WRITE-REGION-IDX
           PERFORM WRITE-DEPT-IDX


           CLOSE F-COMMUNES C-DEPTS C-COMMUNES C-REGIONS
           PERFORM FIN-PGM
           .
      ******************************************************************
      *        FONCTION REGION

       COMPLETE-TABLE-REGION.

           MOVE 1 TO TABLE-INDEX

           PERFORM UNTIL TABLE-INDEX-END-R

               IF R-NOM(TABLE-INDEX) = E-Region THEN

      *            ADD VALUE TO OCCUR
                   ADD 1 TO R-NOMBRE-COMMUNES(TABLE-INDEX)

                   COMPUTE R-SOMME-ALTITUDE(TABLE-INDEX) =
                           R-SOMME-ALTITUDE(TABLE-INDEX)
                           + E-Altitude

                   COMPUTE R-SOMME-SUPERFICIE(TABLE-INDEX) =
                           R-SOMME-SUPERFICIE(TABLE-INDEX)
                           + E-Superficie

                   COMPUTE R-SOMME-POPULATION(TABLE-INDEX) =
                           R-SOMME-POPULATION(TABLE-INDEX)
                           + E-Population

      *            END PERFORM => 18
                   MOVE NOMBRE-REGION TO TABLE-INDEX

               ELSE

                   IF R-INDEXED-COMPLETE-FALSE(TABLE-INDEX) THEN

      *            INDEXED OCCURS
                   MOVE E-Region TO R-NOM(TABLE-INDEX)
                   MOVE 1 TO R-INDEXED-COMPLETE(TABLE-INDEX)

      *            ADD VALUE TO OCCUR
                   ADD 1 TO R-NOMBRE-COMMUNES(TABLE-INDEX)

                   COMPUTE R-SOMME-ALTITUDE(TABLE-INDEX) =
                           R-SOMME-ALTITUDE(TABLE-INDEX) + E-Altitude

                   COMPUTE R-SOMME-SUPERFICIE(TABLE-INDEX) =
                           R-SOMME-SUPERFICIE(TABLE-INDEX)
                           + E-Superficie

                   COMPUTE R-SOMME-POPULATION(TABLE-INDEX) =
                           R-SOMME-POPULATION(TABLE-INDEX)
                           + E-Population

      *            END PERFORM => 18
                   MOVE NOMBRE-REGION TO TABLE-INDEX
                   END-IF

               END-IF
               ADD 1 TO TABLE-INDEX
           END-PERFORM
           .

           COMPLETE-TABLE-DEPT.

           MOVE 1 TO TABLE-INDEX

           PERFORM UNTIL TABLE-INDEX-END-D

               IF D-NOM(TABLE-INDEX) = E-Departement THEN

      *            ADD VALUE TO OCCUR
                   ADD 1 TO D-NOMBRE-COMMUNES(TABLE-INDEX)
                   MOVE E-Region TO D-TABLE-REGION(TABLE-INDEX)

                   COMPUTE D-SOMME-ALTITUDE(TABLE-INDEX) =
                           D-SOMME-ALTITUDE(TABLE-INDEX)
                           + E-Altitude

                   COMPUTE D-SOMME-SUPERFICIE(TABLE-INDEX) =
                           D-SOMME-SUPERFICIE(TABLE-INDEX)
                           + E-Superficie

                   COMPUTE D-SOMME-POPULATION(TABLE-INDEX) =
                           D-SOMME-POPULATION(TABLE-INDEX)
                           + E-Population

      *            END PERFORM => 101
                   MOVE NOMBRE-DEPTS TO TABLE-INDEX

               ELSE

                   IF D-INDEXED-COMPLETE-FALSE(TABLE-INDEX) THEN

      *            INDEXED OCCURS
                   MOVE E-Departement TO D-NOM(TABLE-INDEX)
                   MOVE 1 TO D-INDEXED-COMPLETE(TABLE-INDEX)

      *            ADD VALUE TO OCCUR
                   ADD 1 TO D-NOMBRE-COMMUNES(TABLE-INDEX)

                   COMPUTE D-SOMME-ALTITUDE(TABLE-INDEX) =
                           D-SOMME-ALTITUDE(TABLE-INDEX) + E-Altitude

                   COMPUTE D-SOMME-SUPERFICIE(TABLE-INDEX) =
                           D-SOMME-SUPERFICIE(TABLE-INDEX)
                           + E-Superficie

                   COMPUTE D-SOMME-POPULATION(TABLE-INDEX) =
                           D-SOMME-POPULATION(TABLE-INDEX)
                           + E-Population

      *            END PERFORM => 101
                   MOVE NOMBRE-DEPTS TO TABLE-INDEX
                   END-IF

               END-IF
               ADD 1 TO TABLE-INDEX
           END-PERFORM
           .


      ******************************************************************

       WRITE-COMMUNES-IDX.

           COMPUTE
               C-Taux-Carbone = (3000 - E-Altitude) * 0.0005
           COMPUTE C-Taux-Densite =
               1 + ((E-Population*1000 /E-Superficie)/100)

           MOVE E-Code-Insee TO C-Code-Insee
           MOVE E-Lieu TO C-Lieu
           MOVE E-Donnees TO C-Donnees
           WRITE C-COM-ENREG
               INVALID KEY
                           DISPLAY "Problème d'écriture de clé"
               NOT INVALID KEY
                           CONTINUE
           END-WRITE
           .

       WRITE-DEPT-IDX.

           MOVE 1 TO TABLE-INDEX

           PERFORM UNTIL TABLE-INDEX-END-D

               COMPUTE D-SOMME-ALTITUDE(TABLE-INDEX) =
                       D-SOMME-ALTITUDE(TABLE-INDEX) /
                       D-NOMBRE-COMMUNES(TABLE-INDEX)

               COMPUTE D-SOMME-SUPERFICIE(TABLE-INDEX) =
                       D-SOMME-SUPERFICIE(TABLE-INDEX) /
                       D-NOMBRE-COMMUNES(TABLE-INDEX)

               COMPUTE D-SOMME-POPULATION(TABLE-INDEX) =
                       D-SOMME-POPULATION(TABLE-INDEX) /
                       D-NOMBRE-COMMUNES(TABLE-INDEX)

               COMPUTE D-TABLE-TAUX-CARBONE(TABLE-INDEX) =
                       (3000 - D-SOMME-ALTITUDE(TABLE-INDEX)) * 0.0005

               COMPUTE D-TABLE-TAUX-DENSITE(TABLE-INDEX) =
                       1 + ( (D-SOMME-POPULATION(TABLE-INDEX)*1000) /
                       D-SOMME-SUPERFICIE(TABLE-INDEX))/100

               MOVE D-TABLE-LIEU(TABLE-INDEX) TO D-Lieu
               MOVE D-TABLE-DONNEES(TABLE-INDEX) TO D-Donnees
               MOVE D-TABLE-TAUX(TABLE-INDEX) TO D-Taux
               WRITE C-DEP-ENREG
                   INVALID KEY
                       DISPLAY "Problème d'écriture de clé"
                   NOT INVALID KEY
      *                 DISPLAY C-DEP-ENREG
                        CONTINUE
               END-WRITE
               ADD 1 TO TABLE-INDEX
           END-PERFORM
           .


       WRITE-REGION-IDX.

           MOVE 1 TO TABLE-INDEX
           PERFORM UNTIL TABLE-INDEX-END-R

      *    Calcul de la moyenne et du taux pour chaque region

           DISPLAY R-TAB-LIEU(TABLE-INDEX)

           COMPUTE R-SOMME-ALTITUDE(TABLE-INDEX) =
                   R-SOMME-ALTITUDE(TABLE-INDEX)
                   / R-NOMBRE-COMMUNES(TABLE-INDEX)

           COMPUTE R-SOMME-SUPERFICIE(TABLE-INDEX) =
                   R-SOMME-SUPERFICIE(TABLE-INDEX)
                   / R-NOMBRE-COMMUNES(TABLE-INDEX)

           COMPUTE R-SOMME-POPULATION(TABLE-INDEX) =
                   R-SOMME-POPULATION(TABLE-INDEX)
                   / R-NOMBRE-COMMUNES(TABLE-INDEX)

           COMPUTE R-TAB-TAUX-CARBONE(TABLE-INDEX) =
                   (3000 - R-SOMME-ALTITUDE(TABLE-INDEX)) * 0.0005

           COMPUTE R-TAB-TAUX-DENSITE(TABLE-INDEX) =
                   1 + ( R-SOMME-POPULATION(TABLE-INDEX) * 1000 /
                   R-SOMME-SUPERFICIE(TABLE-INDEX) ) / 100

      *    Ecriture dans le fichier indexé.

           MOVE R-TAB-LIEU(TABLE-INDEX)        TO R-Lieu
           MOVE R-TAB-DONNEES(TABLE-INDEX)     TO R-Donnees
           MOVE R-TAB-TAUX(TABLE-INDEX)        TO R-Taux

           WRITE C-REG-ENREG
               INVALID KEY
                   DISPLAY "REGION INVALID KEY "R-Lieu
               NOT INVALID KEY
                   CONTINUE
           END-WRITE

           ADD 1 TO TABLE-INDEX

           END-PERFORM
           .



      ******************************************************************
       FIN-PGM.
           STOP RUN
           .
      ******************************************************************
