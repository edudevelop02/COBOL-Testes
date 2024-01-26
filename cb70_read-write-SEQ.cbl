       IDENTIFICATION DIVISION.
       PROGRAM-ID. cb70_read-write-SEQ.
      *      
      ****************************************************************
      * Author: Accampora.
      * Date: 12-10-2023.
      * Purpose: Comando read sequential files e gravar
      *  arquivo de saida sequencial.
      *
      ****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---> arquivo de entrada
           SELECT INFL-E ASSIGN TO INFLDD
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-STAT.

      *---> arquivo de saida
           SELECT OUTFL-S ASSIGN TO OUTFLDD
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-OUTFL.
      *
       DATA DIVISION.
       FILE SECTION.
      *  TAMANHO DO REGISTRO = 20 CARACTERES SEM TERMINADOR FISICO
       FD INFL-E.
       01 INFL-REC.
           05 STORE-IDX.
               10 STORE-ID     PIC 9(05).
           05 FILLER       PIC X(01).
           05 ITEM-ID      PIC X(10).
           05 FILLER       PIC X(04).
      *
      *  TAMANHO DO REGISTRO = 20 CARACTERES SEM TERMINADOR FISICO
       FD  OUTFL-S.
       01  OUTFL-REC.
           05 O-STORE-IDX.
              10 O-STORE-ID     PIC 9(05).
           05 DELIMIT-OUT    PIC X(01).
           05 O-ITEM-ID      PIC X(10).
           05 FILLER         PIC X(03).
           05 DELIMIT2-OUT   PIC X(01).
      *
       WORKING-STORAGE SECTION.
       01  INFLDD.
           05  FILLER  PIC X(30) VALUE "./files/cbARQIN.SEQ".

       01  OUTFLDD.
           05  FILLER  PIC X(30) VALUE "./files/cbARQSAIDA.OUT".

       01  FS-STAT           PIC X(02) VALUE SPACES.
           88  FS-STAT-OK    VALUE IS "00".
           88  FS-STAT-EOF   VALUE IS "10".

       01  FS-OUTFL          PIC X(02) VALUE "X".
           88  FS-OUTFL-OK    VALUE IS "00".
           88  FS-OUTFL-EOF   VALUE IS "10".

       01  args-cmd-linex    pic x(50)  VALUE SPACES.

       01  COUNTERS.
           05  READ-COUNT     PIC 9(02).
           05  WRITE-COUNT    PIC 9(02).

           88 E-SIM VALUES ARE "S" "s".
       77  XY     PIC X.
      *
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY "LEITURA SEQUENCIAL E GRAVACAO DE SAIDA"
           " - REGISTRO COM TAMANHO FIXO"
      *====> PEGA ARGUMENTOS DE ENTRADA DA LINHA DE COMANDO SE HOUVER
           ACCEPT args-cmd-linex   from COMMAND-LINE.
           IF  args-cmd-linex  NOT = spaces
               UNSTRING args-cmd-linex DELIMITED BY SPACE
               INTO  INFLDD  OUTFLDD.
      *
           DISPLAY "ABRINDO : " INFLDD
           DISPLAY "SAIDA   : " OUTFLDD
           DISPLAY " >>"
           ACCEPT XY FROM CONSOLE
           PERFORM OPEN-PARA      THRU OPEN-EXIT-PARA
           PERFORM PROCESS-PARA   THRU PROCESS-EXIT-PARA
           PERFORM CLOSE-PARA     THRU CLOSE-EXIT-PARA.
           DISPLAY 'FIM PROG 1 ...'
           STOP RUN.

       OPEN-PARA.
           INITIALIZE FS-STAT FS-OUTFL READ-COUNT WRITE-COUNT.
           OPEN INPUT INFL-E
           IF  FS-STAT-OK
               CONTINUE
           ELSE
               DISPLAY 'ABERTURA DO ARQUIVO FALHOU=' FS-STAT
               "  " INFLDD
               GO TO EXIT-PARA
           END-IF.
      * =====> ABRE ARQUIVO DE SAIDA NO MODO OUTPUT
           OPEN OUTPUT OUTFL-S.
           IF  FS-OUTFL = ZEROS
               DISPLAY "ABRIU SAIDA "
      *         CONTINUE
           ELSE
               DISPLAY 'ABERTURA ARQUIVO SAIDA FALHOU, ST='
               FS-OUTFL  "  FILEL=" OUTFLDD
      *         CLOSE INFL-E
               GO TO EXIT-PARA
           END-IF.

       OPEN-EXIT-PARA.
           EXIT.
      *================================
       PROCESS-PARA.
           PERFORM UNTIL   FS-STAT-EOF
               READ INFL-E
               AT END
                   IF  READ-COUNT < 1
                       DISPLAY 'ARQUIVO VAZIO '
                       GO TO EXIT-PARA
                   END-IF
               NOT AT END
                   PERFORM WRITE-PARA   THRU WRITE-EXIT-PARA
               END-READ
           END-PERFORM.
       PROCESS-EXIT-PARA.
           EXIT.
      *===================================
       WRITE-PARA.
           ADD 1    TO READ-COUNT.
           DISPLAY "==>" INFL-REC "<== REGISTRO=" READ-COUNT
           IF  STORE-ID > 12346
               MOVE SPACES       TO OUTFL-REC
               MOVE STORE-IDX     TO O-STORE-IDX
               MOVE ITEM-ID      TO O-ITEM-ID
               MOVE "|"          TO DELIMIT-OUT
               MOVE "#"          TO DELIMIT2-OUT

      *=====>  GRAVA O REGISTRO ===================
               WRITE OUTFL-REC
               DISPLAY '   STORE-ID: ' STORE-ID
               DISPLAY '   ITEM ID   ' ITEM-ID
           END-IF.
       WRITE-EXIT-PARA.
           EXIT.

       CLOSE-PARA.
           CLOSE INFL-E  OUTFL-S.
       CLOSE-EXIT-PARA.
           EXIT.

      *=================  FIM ======
       EXIT-PARA.
           DISPLAY 'FIM PROG 2 ...'
           EXIT PROGRAM.
