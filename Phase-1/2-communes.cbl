      ******************************************************************
      * Author:    Groupe 3
      * Date:      22/03/2021
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2-COMMUNES.
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      *    Main file
           SELECT  F-COMMUNES ASSIGN TO "2-communes.dat"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT  C-COMMUNES ASSIGN TO "3-communes.dat"
               ORGANIZATION LINE SEQUENTIAL.


      ******************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  F-COMMUNES.
       01  F-COMMUNES-ENREG.

           05 F-Code-Insee.
               10  F-CODE-INSEE-REG    PIC X(2).
                   88  F-CODE-DEPT-CORSE   VALUE  '2A' '2B'.
               10  F-CODE-INSEE-IND    PIC X(3).
           05 FILLER                   PIC X(6).

           05 F-Code-Postal            PIC X(5).
           05 FILLER                   PIC X(28).

           05 F-Lieu.
               10 F-Commune             PIC X(50).
               10 F-Departement         PIC X(28).
               10 F-Region              PIC X(30).

           05 F-Status                 PIC X(16).
           05 FILLER                   PIC X(1).

           05 F-Donnees.
               10 F-Altitude            PIC X(15).
               10 F-Superficie          PIC X(9).
               10 F-Population          PIC X(10).

           05 F-INDEX.
               10 F-CODE-COMMUNE           PIC X(3).
               10 FILLER                   PIC X(11).
               10 F-CODE-CANTON            PIC X(2).
               10 FILLER                   PIC X(10).
               10 F-CODE-ARRON             PIC X(1).
               10 FILLER                   PIC X(17).
               10 F-CODE-DEPARTEMENT       PIC X(2).
               10 FILLER                   PIC X(14).
               10 F-CODE-REGION            PIC X(2).
               10 FILLER                   PIC X(9).

      *     05 F-Taux.
      *         10 F-Taux-Carbone        PIC X(9).
      *         10 F-Taux-Densite        PIC X(9).

       FD  C-COMMUNES.
       01  C-COMMUNES-ENREG.

           05 C-Code-Insee.
               10 C-CODE-INSEE-REG        PIC 9(2).
               10 C-CODE-INSEE-IND        PIC 9(3).

           05 C-Lieu.
               10 C-Commune             PIC X(50).
               10 C-Departement         PIC X(28).
               10 C-Region              PIC X(30).

           05 C-Donnees.
               10 C-Altitude            PIC 9(6)V99 .
               10 C-Superficie          PIC 9(6)V99 .
               10 C-Population          PIC 9(6)V99 .

           05 C-Taux.
               10 C-Taux-Carbone        PIC 9(4)V99 .
               10 C-Taux-Densite        PIC 9(4)V99 .

      ******************************************************************
       WORKING-STORAGE SECTION.

       1   WORKING-DATA-MANAGER.

           05 CODE-INSEE-REG-CORSE     PIC 9(2) VALUE 20.



       1   FILE-WORKING-MANAGER.
      * ++===                                fin article rencontre ===++
           05  FIN-ENREG                PIC  X(01) VALUE  SPACE.
               88  FF                              VALUE  HIGH-VALUE.

      ******************************************************************
       PROCEDURE DIVISION.


       MAIN-PROCEDURE.


           OPEN INPUT F-COMMUNES
           OPEN OUTPUT C-COMMUNES


           PERFORM UNTIL FF

              READ F-COMMUNES
                AT END
                   SET FF TO TRUE
                NOT AT END

                   DISPLAY F-COMMUNES-ENREG

      *            Ajout du code Insee
                   IF F-CODE-DEPT-CORSE THEN
                       MOVE CODE-INSEE-REG-CORSE   TO C-CODE-INSEE-REG
                       MOVE F-CODE-INSEE-IND       TO C-CODE-INSEE-IND
                   ELSE
                       MOVE F-Code-Insee           TO C-Code-Insee
                   END-IF

      *            Ajout du lieu
                   MOVE F-Lieu TO C-Lieu

      *            Ajout des données


      *            Ecriture dans le fichier
                   WRITE C-COMMUNES-ENREG



              END-READ
           END-PERFORM


           CLOSE F-COMMUNES C-COMMUNES
           .

       FIN-PGM.
           STOP RUN.
