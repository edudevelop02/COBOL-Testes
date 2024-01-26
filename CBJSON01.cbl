       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBJSON01.
      **************************************************************
      *   OBS:  comando JSON GENERATE NAO IMPLEMENTADO NO OPECOBOL
      **************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DADOS.
           05  lINHA           PIC X(80)  VALUE SPACES "LINHA 1".
      * Campo tipo NATIONAL reconhece caracteres tipo UTF-16.
       01  JTEXT     NATIONAL  PIC X(2000).
       77  I                   PIC 999.
       77  X                   PIC X.

       PROCEDURE DIVISION.
       MAINX.
           DISPLAY "GERAR DADOS NO FORMATO JSON  EM COBOL"
           JSON GENERATE JTEX FROM DADOS COUNT I
               ON EXCEPTION
                 DISPLAY 'ERRO NA CONVERSAO JSON ' JSON-CODE
               NOT ON EXCEPTION
                 DISPLAY 'JASON CRIADO '
                 DISPLAY JTEXT(1:I)
                 DISPLAY FUNCTION DISPLAY-OF(JTEXT(1:I))
                 DISPLAT 'VALOR DO COUNTER ' I
           END-JSON.

           DISPLAY 'JASON CRIADO '
           STOP RUN.
