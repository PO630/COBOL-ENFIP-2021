       IDENTIFICATION DIVISION.
       PROGRAM-ID. 4-COMMUNES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FE ASSIGN TO "4-communes.dat"
                   ORGANIZATION LINE SEQUENTIAL.

           SELECT FS ASSIGN TO "5-communes.idx"
                   ORGANIZATION INDEXED ACCESS SEQUENTIAL
                   RECORD KEY C-Code-Insee.

       DATA DIVISION.
       FILE SECTION.

       FD  FE.
       01  FE-DATA.
           05 E-Code-Insee  PIC 9(5).
           05 E-Lieu.
               10 E-Commune PIC X(50).
               10 E-Departement PIC X(28).
               10 E-Region PIC X(30).
           05 E-Donnees.
               10 E-Altitude PIC 9(6)V99.
               10 E-Superficie PIC 9(6)V99.
               10 E-Population PIC 9(6)V99.
           05 E-Taux.
               10 E-Taux-Carbone PIC 9(4)V99.
               10 E-Taux-Densite PIC 9(4)V99.

       FD  FS.
       01  FS-DATA.
           05 C-Code-Insee  PIC 9(5).
           05 C-Lieu.
               10 C-Commune PIC X(50).
               10 C-Departement PIC X(28).
               10 C-Region PIC X(30).
           05 C-Donnees.
               10 C-Altitude PIC 9(6)V99.
               10 C-Superficie PIC 9(6)V99.
               10 C-Population PIC 9(6)V99.
           05 C-Taux.
               10 C-Taux-Carbone PIC 9(4)V99.
               10 C-Taux-Densite PIC 9(4)V99.

       WORKING-STORAGE SECTION.

       01  EOF-MANAGER.
           05 FIN-ENREG PIC  X(01) VALUE SPACE.
               88 FF VALUE HIGH-VALUE.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           OPEN INPUT FE
           OPEN OUTPUT FS

           PERFORM UNTIL FF
               READ FE
                   AT END
                       SET FF TO TRUE
                   NOT AT END
                       MOVE E-Lieu TO C-Lieu
                       MOVE E-Donnees TO C-Donnees
                       Move E-Taux TO C-Taux
                       WRITE C-Code-Insee FROM E-Code-Insee
                           INVALID KEY
                           DISPLAY "Problème d'écriture de clé"
                       END-WRITE
                       WRITE C-Lieu
                       WRITE C-Donnees
                       WRITE C-Taux
               END-READ
           END-PERFORM

           CLOSE FE FS

           .

       FIN-PGM.
            STOP RUN.
