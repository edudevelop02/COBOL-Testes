       IDENTIFICATION DIVISION.
       PROGRAM-ID. cbaula72_INDEXED.
      ****************************************************************
      * Author: Accampora.
      * Date: 15-10-2023.
      * Purpose: iNICIALIZA ARQUIVO SE NAO EXISTIR
      *    Ler arquivo indexado com CHAVE DE acesso randomico
      ****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *---> arquivo de entrada
           SELECT INFL ASSIGN TO INFLDD
      *  ==> para ler um arquivo indexado de tamanho de
      *            registro FIXO
      *        declarar:  ORGANIZATION IS INDEXED
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EMP-ID
               FILE STATUS IS FS-INFL.
      *
       DATA DIVISION.
       FILE SECTION.
      *  TAMANHO DO REGISTRO = 50 CARACTERES SEM TERMINADOR FISICO
       FD INFL.
       01  INFL-REC.
               10  EMP-ID.
                   15  IN-PRO-CODIGO       PIC X(05).
               10  EMP-NAME                pic x(40).
               10  REM-BYTE                pic x(05).
      *
       WORKING-STORAGE SECTION.
       01  INFLDD   pic x(30) value "cbAULA72IDX.IDX".
      *     05  FILLER  PIC X(30) VALUE "./file/cbAULA72-IN.DAT".

       01  OUTFLDD.
           05  FILLER  PIC X(30) VALUE "./file/cbAULA72-OUT".

       01  FS-INFL           PIC X(02) VALUE SPACES.
           88  FS-INFL-OK          VALUE IS "00".
           88  FS-INFL-DUP-KEY     VALUE IS "02".
           88  FS-INFL-EOF         VALUE IS "10".

       01  args-cmd-linex    pic x(50)  VALUE SPACES.

       01  COUNTERS.
           05  READ-COUNT     PIC 9(02).
           05  WRITE-COUNT    PIC 9(02).

       77  XY     PIC X.
      *
       PROCEDURE DIVISION.
       INICIO.
           DISPLAY
           "V:2.14 - Ler arquivo indexado com acesso randomico "
           " - REGISTRO COM TAMANHO FIXO"
      *====> PEGA ARGUMENTOS DE ENTRADA DA LINHA DE COMANDO SE HOUVER
      *====> e associa ao arquivo de entrada e de saida
           ACCEPT args-cmd-linex   from COMMAND-LINE.
           IF  args-cmd-linex  NOT = spaces
               UNSTRING args-cmd-linex DELIMITED BY SPACE
               INTO  INFLDD  OUTFLDD.

       OPEN-FILEIN.
           DISPLAY "ABRINDO : " INFLDD
           OPEN INPUT INFL
           DISPLAY "ABRIU INPUT   ST="  FS-INFL
           if  FS-INFL = zeros
               CLOSE INFL
               GO TO CONTINUA
           ELSE
               DISPLAY 'ABERTURA DO ARQUIVO ENTRADA FALHOU=' FS-INFL
               "  " INFLDD
               OPEN OUTPUT INFL
               DISPLAY 'OUTPUT ABERTURA ST= ' FS-INFL
               perform inicializa-arquivo  thru  fim-inicia
               CLOSE INFL
               display "Tecle enter ==>> "
               accept xy from console
               display "<<=== ok "
               STOP RUN
           end-if.
       CONTINUA.
      *     DISPLAY "SAIDA   : " OUTFLDD
           DISPLAY " CONTINUA 01 OPEN >>"
      *     ACCEPT XY FROM CONSOLE
           PERFORM OPEN-PARA      THRU OPEN-EXIT-PARA
           DISPLAY " CONTINUA 02 PROCESS >>"
           PERFORM PROCESS-PARA   THRU PROCESS-EXIT-PARA
           DISPLAY " CONTINUA 03 CLOSE >>"
           PERFORM CLOSE-PARA     THRU CLOSE-EXIT-PARA.
           DISPLAY 'FIM PROG 1 ...'
           STOP RUN.

       OPEN-PARA.
           INITIALIZE FS-INFL READ-COUNT WRITE-COUNT.
           OPEN INPUT INFL
           DISPLAY '01 ABERTURA ST= ' FS-INFL
           if  FS-INFL-OK
               GO TO OPEN-EXIT-PARA
           ELSE
               DISPLAY '01 ABERTURA INPUT DO ARQUIVO FALHOU=' FS-INFL
               "  " INFLDD
               GO TO OPEN-EXIT-PARA
           END-IF.

       OPEN-EXIT-PARA.
           EXIT.
      *================================
       PROCESS-PARA.
           MOVE '00111'    TO EMP-ID
           READ INFL
               KEY IS EMP-ID
               INVALID KEY
                   DISPLAY 'INVALID KEY st=' FS-INFL " KEY=" EMP-ID
               NOT INVALID KEY
                   DISPLAY " CHAVE=" EMP-ID ' DESCRICAO : ' EMP-NAME
                   " ST=" FS-INFL
               END-READ.
       PROCESS-EXIT-PARA.
           EXIT.
      *===================================

       CLOSE-PARA.
           CLOSE INFL.
       CLOSE-EXIT-PARA.
           EXIT.

      *=================  FIM ======
       EXIT-PARA.
           DISPLAY 'FIM PROG 72 ...'
           EXIT PROGRAM.

       inicializa-arquivo.
           OPEN OUTPUT INFL.
           DISPLAY "ABRIU OUTPUT  ST="  FS-INFL
           move spaces     to infl-rec
           MOVE '00111'    TO EMP-ID
           move 'AAAAAAAAAAAAAAAA XXX'  to emp-name
           write infl-rec  invalid key
                 display '1 write erro-st=' FS-INFL
                 stop run.
           display 'gravou 1 ' emp-name " ST=" FS-INFL.

           MOVE '00221'    TO EMP-ID
           move 'BBBBBBBBBBBBBBBBBBBBB'  to emp-name
           write infl-rec  invalid key
                 display '2 write erro-st=' FS-INFL
                 stop run.
           display 'gravou 2 ' emp-name  " ST=" FS-INFL.

           MOVE '00333'    TO EMP-ID
           move 'CCCCCCCCCCCCCCCCCCCCCCCCCC'  to emp-name
           write infl-rec  invalid key
                 display '3 write erro-st=' FS-INFL
                 stop run.
           display 'gravou 3 ' emp-name  " ST=" FS-INFL.
       fim-inicia.
           exit.
      *===================================

      *WRITE-PARA.
      *    ADD 1    TO READ-COUNT.
      *    DISPLAY "==>" INFL-REC "<== REGISTRO=" READ-COUNT
      *    IF  STORE-ID > 12346
      *        MOVE SPACES       TO OUTFL-REC
      *        MOVE STORE-IDX     TO O-STORE-IDX
      *        MOVE ITEM-ID      TO O-ITEM-ID
      *        MOVE "|"          TO DELIMIT-OUT
      *        MOVE "#"          TO DELIMIT2-OUT

      *=====>  GRAVA O REGISTRO ===================
      *        WRITE OUTFL-REC
      *        DISPLAY '   STORE-ID: ' STORE-ID
      *        DISPLAY '   ITEM ID   ' ITEM-ID
      *    END-IF.
      *WRITE-EXIT-PARA.
      *    EXIT.
