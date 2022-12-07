      * COBOL programs have a maximum width of 80 characters.
      * This is so that they could support punch cards (yes, really).
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVENTOFCODE4.
       
      * Other files are referenced here
       ENVIRONMENT DIVISION.
       
       INPUT-OUTPUT SECTION.
       
      * Get the input
       FILE-CONTROL.
       SELECT ADVENTOFCODEINPUT4
       ASSIGN TO "C:\GnuCobol3.1\build\ADVENTOFCODEINPUT4.txt"
       ORGANIZATION IS LINE SEQUENTIAL
       ACCESS MODE IS SEQUENTIAL
       FILE STATUS IS WS-FILE-STATUS.
       
      * Where the variables/data lives
       DATA DIVISION.
       
       FILE SECTION.
       
       FD  ADVENTOFCODEINPUT4.
       01  FILE-INPUT                                   PIC X(12).
       
       WORKING-STORAGE SECTION.
       
      * All COBOL variables must be declared. They have a heirarchical structure.
       01  WS-STRINGS                                   VALUE SPACE.
      * 2 Xs = two characters long
           05  WS-FILE-STATUS                           PIC XX.
      * 88 levels are like if checks
               88  WS-FILE-STATUS-VALID                 VALUE "00".
               88  WS-FILE-STATUS-END                   VALUE "10".
      * This is what a bool looks like in COBOL
           05  WS-PREVIOUS-CHARACTER-WAS-NUMBER         PIC X.
               88  WS-PREVIOUS-CHARACTER-WAS-NUMBER-YES VALUE 'Y'.
           
       01  WS-INTS                                      VALUE ZERO.
      * 3 characters long
           05  WS-NUMBER-OVERLAPPING-ASSIGNMENTS        PIC 9(3).
           05  WS-LOOP-PTR                              PIC 99.
               88  WS-LOOP-PTR-VALID-12                 VALUES 1 THRU 12.
           05  WS-CURRENT-NUMBER. 
               10  WS-CURRENT-NUMBER-TENS-DIGIT         PIC 9.
                   88  WS-CURRENT-NUMBER-TENS-ZERO      VALUE ZERO.
               10  WS-CURRENT-NUMBER-UNITS-DIGIT        PIC 9.
                   88  WS-CURRENT-NUMBER-UNITS-ZERO     VALUE ZERO.
           05  WS-PARSED-INTS.
               10  WS-PREVIOUS-NUMBER                   PIC 9.
                   88  WS-PREVIOUS-NUMBER-ZERO          VALUE ZERO.
               10  WS-ASSIGNMENT-1-START                PIC 99.
                   88  WS-ASSIGNMENT-1-START-ZERO       VALUE ZERO.
               10  WS-ASSIGNMENT-1-END                  PIC 99.
                   88  WS-ASSIGNMENT-1-END-ZERO         VALUE ZERO.
               10  WS-ASSIGNMENT-2-START                PIC 99.
                   88  WS-ASSIGNMENT-2-START-ZERO       VALUE ZERO.
               10  WS-ASSIGNMENT-2-END                  PIC 99.
                   88  WS-ASSIGNMENT-2-END-ZERO         VALUE ZERO.
               
       01  WS-FILE-INPUT                                VALUE SPACE.
      * FILLER is used when you want to add characters, but don't need to 
      * directly reference them (not really needed here)
           05  FILLER                                   PIC X(12).
       
      * REDEFINES lets you rejig the format of the same packet of data
       01  WS-CURRENT                                   REDEFINES 
           WS-FILE-INPUT.
           05  WS-CURRENT-CHARACTER                     PIC X OCCURS 12.
       
       PROCEDURE DIVISION.
       
       PROGRAM-ENTRY.
       
           PERFORM OPEN-FILE.
           PERFORM READ-FILE.
           PERFORM CLOSE-FILE.
           
           DISPLAY "TOTAL NUMBER OVERLAPS: " 
               WS-NUMBER-OVERLAPPING-ASSIGNMENTS.
           
           STOP RUN.
           
       OPEN-FILE.
           OPEN INPUT ADVENTOFCODEINPUT4.
               IF NOT WS-FILE-STATUS-VALID
                   DISPLAY "OPEN FAILED WITH " WS-FILE-STATUS
                   STOP RUN
               END-IF.
             
       READ-FILE.
      * Reads line by line
           PERFORM UNTIL WS-FILE-STATUS-END
               READ ADVENTOFCODEINPUT4
               IF WS-FILE-STATUS-VALID
                   PERFORM POPULATE-WS-DATA
                   PERFORM CHECK-FOR-OVERLAP
               ELSE IF NOT WS-FILE-STATUS-END
                   DISPLAY "READ FAILED WITH " WS-FILE-STATUS
               END-IF
           END-PERFORM.
       
       CLOSE-FILE.
           CLOSE ADVENTOFCODEINPUT4.
           IF NOT WS-FILE-STATUS-VALID
               DISPLAY "FILE CLOSE FAILED WITH " WS-FILE-STATUS
               STOP RUN
           END-IF.
           
      * Our data input is in a terrible format for COBOL because it's variable
      * length. This is very bad. We instead have to loop over every single
      * character to try to parse this into a fixed length format that COBOL
      * can understand.
       POPULATE-WS-DATA SECTION.
           MOVE FILE-INPUT TO WS-FILE-INPUT.
           MOVE ZERO TO WS-PARSED-INTS.
           MOVE SPACE TO WS-PREVIOUS-CHARACTER-WAS-NUMBER.
           MOVE ZERO TO WS-LOOP-PTR.
           
      * Yes, this is what loops look like in COBOL
       POPULATE-WS-DATA-LOOP.
           ADD 1 TO WS-LOOP-PTR.
           IF NOT WS-LOOP-PTR-VALID-12
               GO TO POPULATE-WS-DATA-EXIT
           END-IF.
           
      * Nested if statements are confusing/buggy in COBOL, so best to just use
      * GO TO to workaround this. Won't using GO TO make the code harder to
      * maintain and generally more terrible, you ask? Yes, yes it will.
           IF WS-CURRENT-CHARACTER (WS-LOOP-PTR) IS NUMERIC
               GO TO POPULATE-WS-DATA-NUMBER.
           
      * Some inputs are single digit. If we haven't yet populated the current 
      * nummber, we have one. Do it now, putting it in 09 format.
           IF WS-CURRENT-NUMBER-TENS-ZERO
               AND WS-CURRENT-NUMBER-UNITS-ZERO
               MOVE WS-PREVIOUS-NUMBER TO WS-CURRENT-NUMBER-UNITS-DIGIT
           END-IF
           
           IF WS-ASSIGNMENT-1-START-ZERO
               MOVE WS-CURRENT-NUMBER TO WS-ASSIGNMENT-1-START
           ELSE IF WS-ASSIGNMENT-1-END-ZERO
               MOVE WS-CURRENT-NUMBER TO WS-ASSIGNMENT-1-END
           ELSE IF WS-ASSIGNMENT-2-START-ZERO
               MOVE WS-CURRENT-NUMBER TO WS-ASSIGNMENT-2-START
           ELSE IF WS-ASSIGNMENT-2-END-ZERO
               MOVE WS-CURRENT-NUMBER TO WS-ASSIGNMENT-2-END
      * We're done here, so may as well move on
           ELSE GO TO POPULATE-WS-DATA-EXIT
           END-IF.
           
           MOVE ZERO TO WS-CURRENT-NUMBER.
           MOVE ZERO TO WS-PREVIOUS-NUMBER.
           MOVE SPACE TO WS-PREVIOUS-CHARACTER-WAS-NUMBER.
           
      * DON'T FORGET TO GO TO AT THE END HERE! We'll fall through and create
      * horrific bugs if not :)
           GO TO POPULATE-WS-DATA-LOOP.
           
       POPULATE-WS-DATA-NUMBER.
      * We have a previous number. The numbers are at most 2 digits long, so
      * populate it into our "nice" format
           IF WS-PREVIOUS-CHARACTER-WAS-NUMBER-YES
               MOVE WS-PREVIOUS-NUMBER TO WS-CURRENT-NUMBER-TENS-DIGIT
               MOVE WS-CURRENT-CHARACTER (WS-LOOP-PTR) 
               TO WS-CURRENT-NUMBER-UNITS-DIGIT
           
               MOVE SPACE TO WS-PREVIOUS-CHARACTER-WAS-NUMBER
               MOVE ZERO TO WS-PREVIOUS-NUMBER
           ELSE 
      * We don't know if this is a single number or the first of a pair yet
               MOVE WS-CURRENT-CHARACTER (WS-LOOP-PTR) 
               TO WS-PREVIOUS-NUMBER
               MOVE 'Y' TO WS-PREVIOUS-CHARACTER-WAS-NUMBER
           END-IF.
           
           GO TO POPULATE-WS-DATA-LOOP.
           
       POPULATE-WS-DATA-EXIT.
           EXIT.
       
       CHECK-FOR-OVERLAP SECTION.
      * COBOL is very funny about number formats. It MUST be in format 99.
      * A "fun" bug was COBOL thinking " 7" was greater that "49", because it
      * can't process " 7" properly - it needs "07".
           IF WS-ASSIGNMENT-1-START 
               IS LESS THAN OR EQUAL WS-ASSIGNMENT-2-START
               AND WS-ASSIGNMENT-1-END 
               IS GREATER THAN OR EQUAL WS-ASSIGNMENT-2-END
               ADD 1 TO WS-NUMBER-OVERLAPPING-ASSIGNMENTS
               GO TO CHECK-FOR-OVERLAP-EXIT
           END-IF.
           
           IF WS-ASSIGNMENT-2-START 
               IS LESS THAN OR EQUAL WS-ASSIGNMENT-1-START
               AND WS-ASSIGNMENT-2-END 
               IS GREATER THAN OR EQUAL WS-ASSIGNMENT-1-END
               ADD 1 TO WS-NUMBER-OVERLAPPING-ASSIGNMENTS
           END-IF.
           
      * Final "fun" facts, I couldn't get a debugger to work for this at all. :)
       CHECK-FOR-OVERLAP-EXIT.
           EXIT.
           