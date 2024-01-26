       IDENTIFICATION DIVISION.
       PROGRAM-ID. cbaula74_LER_INDEXED.
      *            123456789.123456789.123
      ****************************************************************
      * Author: Accampora.
      * Date: 15-10-2023.
      * Purpose: Abrir e Ler arquivo indexado
      *
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
               ACCESS MODE IS RANDOM
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
       01  INFLDD   pic x(50) value "cbAULA72IDX.IDX".
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
           "V:1.02 - Ler arquivo indexado com acesso INDEXADO "
           " Randomico "
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
      *        OPEN OUTPUT INFL
      *        DISPLAY 'OUTPUT ABERTURA ST= ' FS-INFL
      *        perform inicializa-arquivo  thru  fim-inicia
      *        CLOSE INFL
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
           DISPLAY 'FIM PROG 74 LER INDEXDADO ...'
           STOP RUN.

       OPEN-PARA.
           INITIALIZE FS-INFL READ-COUNT WRITE-COUNT.
      * =====> abre arquivo no modod ( I-O ) input-Output
           OPEN INPUT INFL
           DISPLAY '01 ABERTURA ST= ' FS-INFL
           if  FS-INFL-OK
               GO TO OPEN-EXIT-PARA
           ELSE
               DISPLAY '01 ABERTURA INPUT DO ARQUIVO FALHOU=' FS-INFL
               "  " INFLDD
               STOP RUN
           END-IF.

       OPEN-EXIT-PARA.
           EXIT.
      *========>> LER O ARQUIVO ATE O FINAL
       PROCESS-PARA.
           READ INFL  NEXT   AT END
                   DISPLAY '**** FIM ARQUIVO DE ENTRADA ST='
                   FS-INFL " ****"
                   display "Tecle enter ==>> "
      *            accept xy line 10 COLUMN 25
                   accept xy
                   display "< OK "
                   GO TO PROCESS-EXIT-PARA
               NOT AT END
                   DISPLAY " CHAVE=" EMP-ID ' DESCRICAO : ' EMP-NAME
                   " ST=" FS-INFL
               END-READ.
           GO TO PROCESS-PARA.

       PROCESS-EXIT-PARA.
           EXIT.
      *===================================

       CLOSE-PARA.
           CLOSE INFL.
       CLOSE-EXIT-PARA.
           EXIT.

      *=================  FIM ======
       EXIT-PARA.
           DISPLAY 'FIM PROG 74 LER INDEXADO ...'
           EXIT PROGRAM.

      *===================================
