      ******************************************************************
      * Author: Kevin Ropital
      * Date: 24/03/2021
      * Purpose: PROJET COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. 4-CONTRIBUABLES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FE ASSIGN TO "4-contribuables.dat"
                   ORGANIZATION LINE SEQUENTIAL.

           SELECT FS ASSIGN TO "5-contribuables.dat"
                   ORGANIZATION LINE SEQUENTIAL.

           SELECT TRI ASSIGN TO DISK.

       DATA DIVISION.
       FILE SECTION.

       FD  FE.
       01  FE-DATA.
           05 E-Identite.
               10 E-Numero-Fiscal PIC 9(8).
               10 E-Prenom PIC X(15).
               10 E-Nom PIC X(11).
               10 E-Code-Insee PIC 9(5).
           05 E-Parts PIC 9v9.

       FD  FS.
       01  FS-DATA.
           05 P-Identite.
               10 P-Numero-Fiscal PIC 9(8).
               10 P-Prenom PIC X(15).
               10 P-Nom PIC X(11).
               10 P-Code-Insee PIC 9(5).
           05 P-Parts PIC 9v9.

       SD  TRI.
       01  T-DATA.
           05  T-Identite.
               10 T-Numero-Fiscal PIC 9(8).
               10 T-Prenom PIC X(15).
               10 T-Nom PIC X(11).
               10 T-Code-Insee PIC 9(5).
           05 T-Parts PIC 9V9.

       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           SORT TRI
               ON ASCENDING KEY T-Numero-Fiscal
               USING FE
               GIVING FS

           .

       FIN-PGM.
            STOP RUN.
