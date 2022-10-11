CREATE OR REPLACE PACKAGE WORDLES IS

  -- AUTHOR  : OLUWAJUWON.ADEMUYIWA
  -- CREATED : 02/03/2022 3:25:24 PM
  -- PURPOSE : IMPLEMENT THE CURRENTLY VIRAL WORDLE ON PL/SQL

  V_USERNAME WORDLE_LOG.OS_USER%TYPE := TRIM(SYS_CONTEXT('USERENV',
                                                                 'OS_USER'));
  V_IP       WORDLE_LOG.IP_ADDRESS%TYPE := TRIM(SYS_CONTEXT('USERENV',
                                                                    'IP_ADDRESS'));

  -- PUBLIC FUNCTION AND PROCEDURE DECLARATIONS
  PROCEDURE WORDLE_3(GUESS_1  IN CHAR,
                     GUESS_2  IN CHAR,
                     GUESS_3  IN CHAR,
                     WORD_1   OUT CHAR,
                     WORD_2   OUT CHAR,
                     WORD_3   OUT CHAR,
                     RESPONSE OUT WORDLE_LOG.RESPONSE%TYPE);

  PROCEDURE WORDLE_5(GUESS_1  IN CHAR,
                     GUESS_2  IN CHAR,
                     GUESS_3  IN CHAR,
                     GUESS_4  IN CHAR,
                     GUESS_5  IN CHAR,
                     WORD_1   OUT CHAR,
                     WORD_2   OUT CHAR,
                     WORD_3   OUT CHAR,
                     WORD_4   OUT CHAR,
                     WORD_5   OUT CHAR,
                     RESPONSE OUT WORDLE_LOG.RESPONSE%TYPE);

END WORDLES;
/
CREATE OR REPLACE PACKAGE BODY WORDLES IS

  --- DECLARATION OF PRIVATE VARIABLES AND PROCEDURES
  LET_1       CHAR(1);
  LET_2       CHAR(1);
  LET_3       CHAR(1);
  LET_4       CHAR(1);
  LET_5       CHAR(1);
  TRIAL_COUNT NUMBER;
  V_FOUND     NUMBER := 0;
  WORD_GUESS  VARCHAR(5);

  CURSOR C_WORD_3 IS
    SELECT WORD FROM WORD_LIST WHERE WORD_TYPE = 3;
  CURSOR C_WORD_5 IS
    SELECT WORD FROM WORD_LIST WHERE WORD_TYPE = 5;
  V_VALID NUMBER;

  --- PROCEDURE TO LOG A VALID GUESS INTO WORDLE_LOG TABLE.
  PROCEDURE INSERT_WORDLE_LOG(RESP   IN WORDLE_LOG.RESPONSE%TYPE,
                              V_TYPE NUMBER) IS
  
  BEGIN
    INSERT INTO WORDLE_LOG
      (IP_ADDRESS, OS_USER, RESPONSE, WORDLE_TYPE, WORDLE_DATE)
    VALUES
      (V_IP, V_USERNAME, RESP, V_TYPE, SYSDATE);
    COMMIT;
  
  EXCEPTION
    WHEN OTHERS THEN
      DBMS_OUTPUT.PUT_LINE(SQLCODE || '------>' || SQLERRM);
      ROLLBACK;
  END INSERT_WORDLE_LOG;

  ---WORDLE PROCEDURE FOR 3 LETTER WORLDS

  PROCEDURE WORDLE_3(GUESS_1  IN CHAR,
                     GUESS_2  IN CHAR,
                     GUESS_3  IN CHAR,
                     WORD_1   OUT CHAR,
                     WORD_2   OUT CHAR,
                     WORD_3   OUT CHAR,
                     RESPONSE OUT WORDLE_LOG.RESPONSE%TYPE) AS
    G1 CHAR := UPPER(GUESS_1);
    G2 CHAR := UPPER(GUESS_2);
    G3 CHAR := UPPER(GUESS_3);
  BEGIN
    ---INSERT WORLDE FOR TODAY FROM WORDLE TABLE.
    SELECT CHAR_1, CHAR_2, CHAR_3
      INTO LET_1, LET_2, LET_3
      FROM WORDLE
     WHERE TRUNC(WORDLE_DATE) = TRUNC(SYSDATE)
     FETCH FIRST 1 ROWS ONLY;
  
    --- GET GUESS TRIAL COUNT FOR USER.
    SELECT COUNT(*)
      INTO TRIAL_COUNT
      FROM WORDLE_LOG
     WHERE OS_USER = V_USERNAME
       AND IP_ADDRESS = V_IP
       AND WORDLE_TYPE = 3
       AND TRUNC(WORDLE_DATE) = TRUNC(SYSDATE);
  
    --- TO CHECK IF THE USER HAS MADE A SUCCESSFUL GUESS FOR THE DAY.
    BEGIN
      SELECT 1
        INTO V_FOUND
        FROM WORDLE_LOG
       WHERE OS_USER = V_USERNAME
         AND WORDLE_TYPE = 3
         AND TRUNC(WORDLE_DATE) = TRUNC(SYSDATE)
         AND RESPONSE = 'SUCCESS!';
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        V_FOUND := 0;
    END;
  
    ---- VALIDATE IF THE GUESS IS A VALID WORD
    SELECT G1 || G2 || G3 INTO WORD_GUESS FROM DUAL;
  
    FOR I IN C_WORD_3 LOOP
      IF WORD_GUESS = I.WORD THEN
        V_VALID := 1;
        EXIT;
      ELSE
        V_VALID := 0;
      END IF;
    END LOOP;
  
    ---COMPARE GUESS WITH WORDLE.
    IF (TRIAL_COUNT <= 6 AND V_FOUND = 0 AND V_VALID = 1) THEN
    
      --- FOR FIRST LETTER
      IF (G1 = LET_1) THEN
        WORD_1 := 'Y';
      ELSIF (G1 = LET_2 OR G1 = LET_3) THEN
        WORD_1 := 'O';
      ELSE
        WORD_1 := 'N';
      END IF;
    
      --- FOR SECOND LETTER
      IF (G2 = LET_2) THEN
        WORD_2 := 'Y';
      ELSIF (G2 = LET_1 OR G2 = LET_3) THEN
        WORD_2 := 'O';
      ELSE
        WORD_2 := 'N';
      END IF;
    
      --- FOR THIRD LETTER
      IF (G3 = LET_3) THEN
        WORD_3 := 'Y';
      ELSIF (G3 = LET_1 OR G3 = LET_2) THEN
        WORD_3 := 'O';
      ELSE
        WORD_3 := 'N';
      END IF;
    
      IF (WORD_1 = 'Y' AND WORD_2 = 'Y' AND WORD_3 = 'Y') THEN
        RESPONSE := 'SUCCESS!';
        INSERT_WORDLE_LOG(RESPONSE, 3);
      ELSE
        RESPONSE := 'TRY AGAIN, YOU ARE SMARTER THAN THIS!';
        INSERT_WORDLE_LOG(RESPONSE, 3);
      END IF;
    
      --- WHEN USER HAS HAD A SUCCESSFUL GUESS PRIOR
    ELSIF (V_FOUND = 1) THEN
      RESPONSE := 'COMPLETED WORDLE FOR TODAY!';
      WORD_1   := NULL;
      WORD_2   := NULL;
      WORD_3   := NULL;
    
      --- WHEN WORD IS NOT VALID.  
    ELSIF (V_VALID = 0) THEN
      RESPONSE := 'NOT A VALID WORD';
      WORD_1   := NULL;
      WORD_2   := NULL;
      WORD_3   := NULL;
    
      --- USER HAS EXCEEDED GUESSES FOR THE DAY.  
    ELSE
      RESPONSE := 'EXCEEDED TRIALS FOR TODAY';
      WORD_1   := NULL;
      WORD_2   := NULL;
      WORD_3   := NULL;
    END IF;
  
  EXCEPTION
    WHEN OTHERS THEN
      DBMS_OUTPUT.PUT_LINE(SQLCODE || '------>' || SQLERRM);
      RESPONSE := SQLERRM;
    
  END WORDLE_3;

  PROCEDURE WORDLE_5(GUESS_1  IN CHAR,
                     GUESS_2  IN CHAR,
                     GUESS_3  IN CHAR,
                     GUESS_4  IN CHAR,
                     GUESS_5  IN CHAR,
                     WORD_1   OUT CHAR,
                     WORD_2   OUT CHAR,
                     WORD_3   OUT CHAR,
                     WORD_4   OUT CHAR,
                     WORD_5   OUT CHAR,
                     RESPONSE OUT WORDLE_LOG.RESPONSE%TYPE) AS
  BEGIN
    ---INSERT WORLDE FOR TODAY FROM WORDLE TABLE.
    SELECT CHAR_1, CHAR_2, CHAR_3, CHAR_4, CHAR_5
      INTO LET_1, LET_2, LET_3, LET_4, LET_5
      FROM WORDLE_FIVE
     WHERE TRUNC(WORDLE_DATE) = TRUNC(SYSDATE)
     FETCH FIRST 1 ROWS ONLY;
  
    --- GET GUESS TRIAL COUNT FOR USER.
    SELECT COUNT(*)
      INTO TRIAL_COUNT
      FROM WORDLE_LOG
     WHERE OS_USER = V_USERNAME
       AND IP_ADDRESS = V_IP
       AND WORDLE_TYPE = 5
       AND TRUNC(WORDLE_DATE) = TRUNC(SYSDATE);
  
    --- TO CHECK IF THE USER HAS MADE A SUCCESSFUL GUESS FOR THE DAY.
    BEGIN
      SELECT 1
        INTO V_FOUND
        FROM WORDLE_LOG
       WHERE OS_USER = V_USERNAME
         AND WORDLE_TYPE = 5
         AND TRUNC(WORDLE_DATE) = TRUNC(SYSDATE)
         AND RESPONSE = 'SUCCESS!';
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        V_FOUND := 0;
    END;
  
    ---- VALIDATE IF THE GUESS IS A VALID WORD
    SELECT GUESS_1 || GUESS_2 || GUESS_3 || GUESS_4 || GUESS_5
      INTO WORD_GUESS
      FROM DUAL;
  
    FOR I IN C_WORD_5 LOOP
      IF WORD_GUESS = I.WORD THEN
        V_VALID := 1;
        EXIT;
      ELSE
        V_VALID := 0;
      END IF;
    END LOOP;
  
    ---COMPARE GUESS WITH WORDLE.
    IF (TRIAL_COUNT <= 6 AND V_FOUND = 0 AND V_VALID = 1) THEN
    
      --- FOR FIRST LETTER
      IF (GUESS_1 = LET_1) THEN
        WORD_1 := 'Y';
      ELSIF (GUESS_1 = LET_2 OR GUESS_1 = LET_3 OR GUESS_1 = LET_4 OR
            GUESS_1 = LET_5) THEN
        WORD_1 := 'O';
      ELSE
        WORD_1 := 'N';
      END IF;
    
      --- FOR SECOND LETTER
      IF (GUESS_2 = LET_2) THEN
        WORD_2 := 'Y';
      ELSIF (GUESS_2 = LET_1 OR GUESS_2 = LET_3 OR GUESS_2 = LET_4 OR
            GUESS_2 = LET_5) THEN
        WORD_2 := 'O';
      ELSE
        WORD_2 := 'N';
      END IF;
    
      --- FOR THIRD LETTER
      IF (GUESS_3 = LET_3) THEN
        WORD_3 := 'Y';
      ELSIF (GUESS_3 = LET_1 OR GUESS_3 = LET_2 OR GUESS_3 = LET_4 OR
            GUESS_3 = LET_5) THEN
        WORD_3 := 'O';
      ELSE
        WORD_3 := 'N';
      END IF;
    
      --- FOR FORTH LETTER
      IF (GUESS_4 = LET_4) THEN
        WORD_4 := 'Y';
      ELSIF (GUESS_4 = LET_1 OR GUESS_4 = LET_2 OR GUESS_4 = LET_3 OR
            GUESS_4 = LET_5) THEN
        WORD_4 := 'O';
      ELSE
        WORD_4 := 'N';
      END IF;
    
      --- FOR FIFTH LETTER
      IF (GUESS_5 = LET_5) THEN
        WORD_5 := 'Y';
      ELSIF (GUESS_5 = LET_1 OR GUESS_5 = LET_2 OR GUESS_5 = LET_3 OR
            GUESS_5 = LET_4) THEN
        WORD_5 := 'O';
      ELSE
        WORD_5 := 'N';
      END IF;
    
      IF (WORD_1 = 'Y' AND WORD_2 = 'Y' AND WORD_3 = 'Y' AND WORD_4 = 'Y' AND
         WORD_5 = 'Y') THEN
        RESPONSE := 'SUCCESS!';
        INSERT_WORDLE_LOG(RESPONSE, 5);
      ELSE
        RESPONSE := 'TRY AGAIN, YOU ARE SMARTER THAN THIS!';
        INSERT_WORDLE_LOG(RESPONSE, 5);
      END IF;
    
      --- WHEN USER HAS HAD A SUCCESSFUL GUESS PRIOR
    ELSIF (V_FOUND = 1) THEN
      RESPONSE := 'COMPLETED WORDLE FOR TODAY!';
      WORD_1   := NULL;
      WORD_2   := NULL;
      WORD_3   := NULL;
      WORD_4   := NULL;
      WORD_5   := NULL;
    
      --- WHEN WORD IS NOT VALID.  
    ELSIF (V_VALID = 0) THEN
      RESPONSE := 'NOT A VALID WORD';
      WORD_1   := NULL;
      WORD_2   := NULL;
      WORD_3   := NULL;
      WORD_4   := NULL;
      WORD_5   := NULL;
    
      --- USER HAS EXCEEDED GUESSES FOR THE DAY.  
    ELSE
      RESPONSE := 'EXCEEDED TRIALS FOR TODAY';
      WORD_1   := NULL;
      WORD_2   := NULL;
      WORD_3   := NULL;
      WORD_4   := NULL;
      WORD_5   := NULL;
    END IF;
  
  EXCEPTION
    WHEN OTHERS THEN
      DBMS_OUTPUT.PUT_LINE(SQLCODE || '------>' || SQLERRM);
      RESPONSE := SQLERRM;
    
  END WORDLE_5;

END WORDLES;
/
