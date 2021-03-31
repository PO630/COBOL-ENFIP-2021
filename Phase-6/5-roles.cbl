      ******************************************************************
      * Author:        Marcheix François-Xavier
      * Date:          23/03/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 5-ROLES.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    Phase-2-file
           SELECT  F-COMMUNES ASSIGN TO "5-communes.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS C-Code-Insee.

           SELECT  F-DEPTS ASSIGN TO "5-depts.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS D-Departement.

           SELECT  F-REGIONS ASSIGN TO "5-regions.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS R-Region.

      *    Phase-3-file
           SELECT  F-CONTRIBUABLES ASSIGN TO "5-contribuables.dat"
               ORGANIZATION LINE SEQUENTIAL.

      *    Phase-4-file
           SELECT  F-OCCURRENCES ASSIGN TO "5-occurrences.idx"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS F-OCC-PRIMARY-KEY
               ALTERNATE RECORD KEY O-Numero-Fiscal WITH DUPLICATES.

      *    Phase-6-return
           SELECT  C-EXONERES ASSIGN TO "6-exoneres.dat"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT  C-ANOMALIES ASSIGN TO "6-anomalies.dat"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT  C-IMPOSABLES ASSIGN TO "6-imposables.dat"
               ORGANIZATION LINE SEQUENTIAL.


      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

      *    Phase-2-file
       FD  F-COMMUNES.
       01  F-COM-ENREG.
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

       FD  F-DEPTS.
       01  F-DEP-ENREG.
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


       FD  F-REGIONS.
       01  F-REG-ENREG.
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

      *    Phase-3-file
       FD  F-CONTRIBUABLES.
       01  F-CON-ENREG.
           05 P-Identite.
             10 P-Numero-Fiscal   PIC 9(8).
             10 P-Prenom          PIC X(15).
             10 P-Nom             PIC X(11).
             10 P-Code-Insee      PIC 9(5).
           05 P-Parts             PIC 9v9.

      *    Phase-4-file
       FD  F-OCCURRENCES.
       01  F-OCC-ENREG.
           05 F-OCC-PRIMARY-KEY     PIC 9(4).
           05 O-Numero-Fiscal  PIC 9(8).
           05 O-Occurrence.
               10 O-Taxe       PIC X(2).
                   88 O-Taxe-carbone   VALUE "TF".
                   88 O-Taxe-densite   VALUE "TH".
                   88 O-Taxe-revenu    VALUE "IR".
               10 O-Revenu     PIC 9(6).
           05 O-Code-Insee     PIC 9(5).

      *    Phase-6-return
       FD  C-EXONERES.
       01  C-EXO-ENREG.
           05 E-Identite.
             10 E-Numero-Fiscal   PIC 9(8).
             10 E-Prenom          PIC X(15).
             10 E-Nom             PIC X(11).
             10 E-Code-Insee      PIC 9(5).
           05 E-Lieu.
               10 E-Commune       PIC X(50).
               10 E-Departement   PIC X(28).
               10 E-Region        PIC X(30).
           05 E-Imposition.
               10 E-Impot         PIC 9(6).
               10 E-Impot-Commune PIC 9(6).
               10 E-Impot-Dept    PIC 9(6).
               10 E-Impot-Region  PIC 9(6).
               10 E-Occurrences   PIC 9(2).

       FD  C-ANOMALIES.
       01  C-ANO-ENREG.
           05 A-Identite.
               10 A-Numero-Fiscal PIC 9(8).
               10 A-Prenom        PIC X(15).
               10 A-Nom           PIC X(11).
               10 A-Code-Insee    PIC 9(5).
           05 A-Lieu.
               10 A-Commune       PIC X(50).
               10 A-Departement   PIC X(28).
               10 A-Region        PIC X(30).
           05 A-Occurrence.
               10 A-Taxe          PIC X(2).
               10 A-Revenu        PIC 9(6).
           05 A-Erreur            PIC X(50).

       FD  C-IMPOSABLES.
       01  C-IMP-ENREG.
           05 I-Identite.
             10 I-Numero-Fiscal   PIC 9(8).
             10 I-Prenom          PIC X(15).
             10 I-Nom             PIC X(11).
             10 I-Code-Insee      PIC 9(5).
           05 I-Lieu.
               10 I-Commune       PIC X(50).
               10 I-Departement   PIC X(28).
               10 I-Region        PIC X(30).
           05 I-Imposition.
               10 I-Impot         PIC 9(6).
               10 I-Impot-Commune PIC 9(6).
               10 I-Impot-Dept    PIC 9(6).
               10 I-Impot-Region  PIC 9(6).
               10 I-Occurrences   PIC 9(2).

      ******************************************************************
       WORKING-STORAGE SECTION.

       1   WORKING-DATA-MANAGER.

      *    Debug on
           05 Debug-boolean    PIC 9(1) VALUE 0.
               88 debug-full   VALUE 1.

      *    Anomalies
           05 ANOMALIES-FOUND           PIC 9(1) VALUE 0.
               88  ANOMALIES-FOUND-FALSE       VALUE 0.
               88  ANOMALIES-FOUND-COMMUNE     VALUE 1.
               88  ANOMALIES-FOUND-DEPARTEMENT VALUE 2.
               88  ANOMALIES-FOUND-REGION      VALUE 3.
               88  ANOMALIES-FOUND-OCCURENCE   VALUE 4.
               88  ANOMALIES-FOUND-TYPE        VALUE 5.

           05 taxe-info.
               10 quotient             PIC 9(6).
               10 taxe-Commune         PIC 9(6).
               10 taxe-Dept            PIC 9(6).
               10 taxe-Region          PIC 9(6).
               10 taxe                 PIC 9(6).
               10 Revenu               PIC 9(6).
      *        A-Erreur
      *        Erreur-Occurrence

       1   FILE-WORKING-MANAGER.
      * ++===                                fin article rencontre ===++
      *    ENREG-CONTRIBUABLES
           05  FIN-ENREG                PIC  X(01) VALUE  SPACE.
               88  FF                              VALUE  HIGH-VALUE.
      *    ENREG-OCCURRENCES
           05  FIN-ENREG-OCC            PIC  X(01) VALUE  SPACE.
               88  FF-OCC                          VALUE  HIGH-VALUE.

      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

      *    Open file
           OPEN INPUT
               F-COMMUNES F-DEPTS F-REGIONS
               F-CONTRIBUABLES F-OCCURRENCES

           OPEN OUTPUT
               C-EXONERES C-ANOMALIES C-IMPOSABLES

      *    Read F-CONTRIBUABLE
           PERFORM UNTIL FF

              READ F-CONTRIBUABLES
                AT END
                   SET FF TO TRUE

                NOT AT END
                   PERFORM INITIALIZE-DATA
                   PERFORM LOAD-DATA-CONTRIBUABLE

              END-READ
           END-PERFORM


      *    Close File
           CLOSE
               F-COMMUNES F-DEPTS F-REGIONS
               F-CONTRIBUABLES F-OCCURRENCES
               C-EXONERES C-ANOMALIES C-IMPOSABLES

      *    End Programme
           PERFORM FIN-PGM
           .

      *===============================================================*
      *    MAIN-FONCTION


       INITIALIZE-DATA.

      *    Erreur init
           INITIALIZE ANOMALIES-FOUND
           INITIALIZE A-Identite A-Lieu A-Occurrence A-Erreur
      *    file
           MOVE SPACE TO FIN-ENREG-OCC
      *    calcul init
           INITIALIZE
               quotient taxe-Commune taxe-Dept taxe-Region taxe
               Revenu
      *    resultat init
           INITIALIZE I-Identite E-Identite
           INITIALIZE I-Lieu E-Lieu
           INITIALIZE I-Imposition E-Imposition
           .

       LOAD-DATA-CONTRIBUABLE.

      *        Lecture des informations communes-departement-region
               PERFORM READ-COMMUNES
      *        Si lecture sans anomalies
               IF ANOMALIES-FOUND-FALSE THEN
      *        Lecture de chaque occurences pour le contribuable
                      PERFORM READ-OCCURENCES
               END-IF
           .

      *===============================================================*
      *                        / READ FILE INFO /

       READ-COMMUNES.

           MOVE P-Code-Insee TO C-Code-Insee

           READ F-COMMUNES

               INVALID KEY
      *            Il n'existe aucune commune pour code-insee
                   PERFORM WRITE-ANOMALIES-COMMUNE

               NOT INVALID KEY

      *            READ DEPTS
                   MOVE C-Departement TO D-Departement
                   READ F-DEPTS
                   INVALID KEY
      *                Il n'existe aucun deartement pour la clé
                       PERFORM WRITE-ANOMALIES-DEPT
                   NOT INVALID KEY
                           CONTINUE
                   END-READ

      *            READ REGION
                   MOVE C-Region TO R-Region
                   READ F-REGIONS
                   INVALID KEY
      *                Il n'existe aucune region pour la clé
                       PERFORM WRITE-ANOMALIES-REGION
                   NOT INVALID KEY
                           CONTINUE
                   END-READ

           END-READ
       .


       READ-OCCURENCES.

           MOVE P-Numero-Fiscal TO O-Numero-Fiscal

           READ F-OCCURRENCES KEY IS O-Numero-Fiscal

               INVALID KEY
      *            Il n'existe aucun occurence pour numero-fiscal
                   PERFORM WRITE-ANOMALIES-OCCURENCE

               NOT INVALID KEY
      *            On traite chaque occurence jusqu'au prochain num

               PERFORM UNTIL (
               ( P-Numero-Fiscal NOT = O-Numero-Fiscal ) OR FF-OCC )

      *                    Traitement de l'impot
                           PERFORM CALCUL-OCCURENCES

      *                    read next value
                           READ F-OCCURRENCES NEXT
                           AT END
                               SET FF-OCC TO TRUE
                           END-READ

               END-PERFORM

               PERFORM Calcul-IR
               MOVE I-Imposition TO E-Imposition

               IF ANOMALIES-FOUND-FALSE THEN
      *        Ecriture sur le fichier
                      IF I-Impot < 1000 THEN
      *            Exoneres.dat
                          WRITE C-EXO-ENREG
                          END-WRITE
                      ELSE
      *            imposables.dat
                          WRITE C-IMP-ENREG
                          END-WRITE
                      END-IF
               END-IF

           END-READ
           .

      *===============================================================*
      *                        / CALCUL OCCURENCES /

       CALCUL-OCCURENCES.

           MOVE P-Identite TO I-Identite
           MOVE P-Identite TO E-Identite

           MOVE C-Lieu TO I-Lieu
           MOVE C-Lieu TO E-Lieu

           EVALUATE TRUE
               WHEN O-Taxe-carbone
                   PERFORM Calcul-TF
               WHEN O-Taxe-densite
                   PERFORM Calcul-TH
               WHEN O-Taxe-revenu
                   ADD O-Revenu TO Revenu
               WHEN OTHER
                   PERFORM WRITE-ANOMALIES-TYPE
           END-EVALUATE

           MOVE I-Imposition TO E-Imposition

           ADD 1 TO E-Occurrences
           ADD 1 TO I-Occurrences
           .

      *===============================================================*
      *                        / WRITE ANOMALIES /


       WRITE-ANOMALIES-COMMUNE.

           MOVE 1 TO ANOMALIES-FOUND
           INITIALIZE A-Identite A-Lieu A-Occurrence A-Erreur
           MOVE P-Identite TO A-Identite

           STRING "communes introuvable " P-Code-Insee
               INTO A-Erreur.

           WRITE C-ANO-ENREG
           END-WRITE
           .

       WRITE-ANOMALIES-DEPT.

           MOVE 2 TO ANOMALIES-FOUND
           INITIALIZE A-Identite A-Lieu A-Occurrence A-Erreur
           MOVE P-Identite TO A-Identite
           MOVE C-Lieu TO A-Lieu

           STRING "departement introuvable " C-Departement
               INTO A-Erreur.

           WRITE C-ANO-ENREG
           END-WRITE
           .

       WRITE-ANOMALIES-REGION.

           MOVE 3 TO ANOMALIES-FOUND
           INITIALIZE A-Identite A-Lieu A-Occurrence A-Erreur
           MOVE P-Identite TO A-Identite
           MOVE C-Lieu TO A-Lieu

           STRING "region introuvable " C-Region
               INTO A-Erreur.

           WRITE C-ANO-ENREG
           END-WRITE
           .

       WRITE-ANOMALIES-OCCURENCE.

           MOVE 4 TO ANOMALIES-FOUND
           INITIALIZE A-Identite A-Lieu A-Occurrence A-Erreur
           MOVE P-Identite TO A-Identite
           MOVE C-Lieu TO A-Lieu

           MOVE "aucun occurence trouvee" TO A-Erreur.

           WRITE C-ANO-ENREG
           END-WRITE
           .


       WRITE-ANOMALIES-TYPE.

           MOVE 5 TO ANOMALIES-FOUND
           INITIALIZE A-Identite A-Lieu A-Occurrence A-Erreur
           MOVE P-Identite TO A-Identite
           MOVE C-Lieu TO A-Lieu
           MOVE O-Occurrence TO A-Occurrence

           STRING "occurence de type " O-Taxe
               INTO A-Erreur.

           WRITE C-ANO-ENREG
           END-WRITE
           .

      *===============================================================*
      *                        / CALCUL IMPOTS /

       Calcul-IR.
           MOVE 0 TO taxe
           DIVIDE Revenu BY P-Parts GIVING quotient ROUNDED
           ON SIZE ERROR
               DISPLAY "Dépassement sur Revenu (Calcul-IR)"
               STOP RUN
           END-DIVIDE

      *    Tranche
           IF  quotient >  156244
               COMPUTE  taxe =
                   taxe + ((quotient - 156244) * P-Parts * 45 / 100)
                   ON SIZE ERROR
                       DISPLAY "Dépassement sur taxe (Calcul-IR):36"
                       STOP RUN
               END-COMPUTE
               MOVE    156244  TO  quotient
           END-IF

           IF  quotient >   73779
               COMPUTE  taxe =
                   taxe + ((quotient -  73779) * P-Parts * 41 / 100)
                   ON SIZE ERROR
                       DISPLAY "Dépassement sur taxe (Calcul-IR):44"
                       STOP RUN
               END-COMPUTE
               MOVE     73779  TO  quotient
           END-IF

           IF  quotient >   27519
               COMPUTE  taxe =
                   taxe + ((quotient -  27519) * P-Parts * 30 / 100)
                   ON SIZE ERROR
                       DISPLAY "Dépassement sur taxe (Calcul-IR):56"
                       STOP RUN
               END-COMPUTE
               MOVE     27519  TO  quotient
           END-IF

           IF  quotient >   9964
               COMPUTE  taxe =
                   taxe + ((quotient -  9964)  * P-Parts * 14 / 100)
                   ON SIZE ERROR
                       DISPLAY "Dépassement sur taxe (Calcul-IR):66"
                       STOP RUN
               END-COMPUTE
               MOVE     9964  TO  quotient
           END-IF

      *    Calcule les trois taxes
           COMPUTE taxe-Commune = taxe * 30 / 100
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Commune (Calcul-IR)"
                   STOP RUN
           END-COMPUTE
           COMPUTE taxe-Dept    = taxe * 20 / 100
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Dept (Calcul-IR)"
                   STOP RUN
           END-COMPUTE
           COMPUTE taxe-Region  = taxe * 50 / 100
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Region (Calcul-IR)"
                   STOP RUN
           END-COMPUTE


           IF debug-full THEN
               DISPLAY "IR Revenu=" Revenu ", Parts=" P-Parts
                   WITH NO ADVANCING
           END-IF
           PERFORM Calcul-Impot
           .

       Calcul-TF.
      *    Calcule les trois taxes
           COMPUTE taxe-Commune = (O-Revenu * C-Taux-Carbone / 100)
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Commune (Calcul-TF)"
                   STOP RUN
           END-COMPUTE
           COMPUTE taxe-Dept    = (O-Revenu * D-Taux-Carbone / 100)
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Dept (Calcul-TF)"
                   STOP RUN
           END-COMPUTE
           COMPUTE taxe-Region  = (O-Revenu * R-Taux-Carbone / 100)
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Region (Calcul-TF)"
                   STOP RUN
           END-COMPUTE

           IF debug-full THEN
               DISPLAY "TF R=" O-Revenu
                       ", TxComm=" C-Taux-Carbone
                       ", TxDept=" D-Taux-Carbone
                       ", TxReg=" R-Taux-Carbone
                   WITH NO ADVANCING
           END-IF
           PERFORM Calcul-Impot
           .

       Calcul-TH.
      *    Calcule les trois taxes
           COMPUTE taxe-Commune = (O-Revenu * C-Taux-Densite / 100)
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Commune (Calcul-TH)"
                   STOP RUN
           END-COMPUTE
           COMPUTE taxe-Dept    = (O-Revenu * D-Taux-Densite / 100)
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Dept (Calcul-TH)"
                   STOP RUN
           END-COMPUTE
           COMPUTE taxe-Region  = (O-Revenu * R-Taux-Densite / 100)
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe-Region (Calcul-TH)"
                   STOP RUN
           END-COMPUTE

           IF debug-full THEN
               DISPLAY "TH R=" O-Revenu
                       ", TxComm=" C-Taux-Densite
                       ", TxDept=" D-Taux-Densite
                       ", TxReg=" R-Taux-Densite
                   WITH NO ADVANCING
           END-IF
           PERFORM Calcul-Impot
           .

       Calcul-Impot.

           COMPUTE taxe = taxe-Commune +
                   taxe-Dept +
                   taxe-Region
               ON SIZE ERROR
                   DISPLAY "Dépassement sur taxe (Calcul-Impot):151"
                   STOP RUN
           END-COMPUTE

      *    Calcule l'impot total
           IF debug-full THEN
               DISPLAY ", Taxe=" taxe ", TaxeComm=" taxe-Commune
                   ", TaxeDept=" taxe-Dept ", TaxeReg=" taxe-Region
           END-IF

      *    Comptabilise les taxes du contribuable
           ADD taxe         TO I-Impot
               ON SIZE ERROR
                 DISPLAY "Dépassement sur taxe (Calcul-Impot):166"
                 STOP RUN
           END-ADD
           ADD taxe-Commune TO I-Impot-Commune
               ON SIZE ERROR
                 DISPLAY "Dépassement sur taxe-Commune (Calcul-Impot)"
                 STOP RUN
           END-ADD
           ADD taxe-Dept    TO I-Impot-Dept
               ON SIZE ERROR
                 DISPLAY "Dépassement sur taxe-Dept (Calcul-Impot)"
                 STOP RUN
           END-ADD
           ADD taxe-Region  TO I-Impot-Region
               ON SIZE ERROR
                 DISPLAY "Dépassement sur taxe-Region (Calcul-Impot)"
                 STOP RUN
           END-ADD
           .

      *===============================================================*
       FIN-PGM.
           STOP RUN.
      *===============================================================*
