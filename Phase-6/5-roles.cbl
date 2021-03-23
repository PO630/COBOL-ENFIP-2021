      ******************************************************************
      * Author:        Marcheix François-Xavier
      * Date:          23/03/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
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
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS F-OCC-PRIMARY-KEY.

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
               10 O-Revenu     PIC 9(6).
               10 O-Code-Insee PIC 9(5).

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

      *    Anomalies
           05 ANOMALIES-FOUND           PIC 9(1) VALUE 0.
               88  ANOMALIES-FOUND-FALSE       VALUE 0.
               88  ANOMALIES-FOUND-COMMUNE     VALUE 1.
               88  ANOMALIES-FOUND-DEPARTEMENT VALUE 2.
               88  ANOMALIES-FOUND-REGION      VALUE 3.
               88  ANOMALIES-FOUND-OCCURENCE   VALUE 4.
               88  ANOMALIES-FOUND-AUCUNE      VALUE 5.

       1   FILE-WORKING-MANAGER.
      * ++===                                fin article rencontre ===++
           05  FIN-ENREG                PIC  X(01) VALUE  SPACE.
               88  FF                              VALUE  HIGH-VALUE.

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

               PERFORM INIT-TRAITEMENT-CONTRIBUABLE

               PERFORM LOAD-LIEU-DATA

              END-READ
           END-PERFORM



      *    Close File
           CLOSE
               F-COMMUNES F-DEPTS F-REGIONS
               F-CONTRIBUABLES F-OCCURRENCES
               C-EXONERES C-ANOMALIES C-IMPOSABLES

      *    End Programme
           PERFORM FIN-PGM.
           .

      *===============================================================*
      *    FONCTION

       INIT-TRAITEMENT-CONTRIBUABLE.
           INITIALIZE ANOMALIES-FOUND
           .

       LOAD-LIEU-DATA.
      *        READ COMMUNES
               MOVE P-Code-Insee TO C-Code-Insee
               READ F-COMMUNES
               INVALID KEY
                   MOVE 1 TO ANOMALIES-FOUND
               NOT INVALID KEY
      *            READ DEPTS
                   MOVE C-Departement TO D-Departement
                   READ F-DEPTS
                   INVALID KEY
                       MOVE 2 TO ANOMALIES-FOUND
                   NOT INVALID KEY
                           CONTINUE
                   END-READ
      *            READ REGION
                   MOVE C-Region TO R-Region
                   READ F-REGIONS
                   INVALID KEY
                       MOVE 3 TO ANOMALIES-FOUND
                   NOT INVALID KEY
                           CONTINUE
                   END-READ
               END-READ
           .



      *===============================================================*
       FIN-PGM.
           STOP RUN.
      *===============================================================*
