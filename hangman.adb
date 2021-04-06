-- Program: hangman.adb
-- Adapted from code from cis3190 at University of Guelph
-- Written by Ian McKechnie (imckechn | 1051662)

with ada.Text_IO; use Ada.Text_IO;
with ada.numerics.discrete_random;

procedure Hangman is
    type randRange is range 1..50;
    type pic is array(integer range 1..12, integer range 1..12) of Character;
    type wordsType is array(1..50) of string(1..20);
    type wordLengthsType is array(1..50) of integer;

    package Rand_Int is new ada.numerics.discrete_random(randRange);
    use Rand_Int;
    num : randRange;

    --Variable declaration
    picture: pic;
    lettersUsed: string(1..20);
    lettersAlreadyGuessed: string(1..26);
    word: string(1..20);
    guess: Character;
    wordGuess: string(1..20);

    counter, errorCount, guessCount, correctGuess, lengthSpot, wordLength: integer;
    checker: integer;

    words: constant wordsType := (
        "gum                 ", "sin                 ", "for                 ",
        "cry                 ", "lug                 ", "bye                 ",
        "fly                 ", "ugly                ", "each                ",
        "from                ", "work                ", "talk                ",
        "with                ", "self                ", "pizza               ",
        "thing               ", "feign               ", "fiend               ",
        "elbow               ", "fault               ", "dirty               ",
        "budget              ", "spirit              ", "quaint              ",
        "maiden              ", "escort              ", "pickax              ",
        "example             ", "tension             ", "quinine             ",
        "kidney              ", "replica             ", "sleeper             ",
        "triangle            ", "kangaroo            ", "mahogany            ",
        "sergeant            ", "sequence            ", "moustache           ",
        "dangerous           ", "scientist           ", "different           ",
        "quiescent           ", "magistrate          ", "erroneously         ",
        "loudspeaker         ", "phytotoxic          ", "matrimonial         ",
        "parasympathomimetic ", "thigmotropism       "
    );

    wordLengths : constant wordLengthsType := (
        3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5,
        6, 6, 6, 6, 6, 6, 7, 7, 7, 6, 7, 7, 8, 8, 8, 8, 8, 9, 9, 9, 9,
        9, 10, 11, 11, 10, 11, 19, 13
    );

    --I use this to get the user input on if they want to play again
    procedure getInput(guess: in out character) is
    begin
        while True loop
            put_line("Want to play again (y/n): ");
            get(guess);
            skip_line;

            exit when (guess = 'y');
            exit when (guess = 'n');
        end loop;
    end getInput;

    --I use this to set up all the variables for a new game -> IE set them to their default values
    procedure setup(picture : out pic; wordGuess, lettersAlreadyGuessed, lettersUsed, word: out string;
        errorCount, correctGuess, guessCount: out Integer; lengthSpot: out Integer; wordLengths: in wordLengthsType; words: in wordsType) is
        gen : Generator;
    begin

        for i in 1..12 loop
            for j in 1..12 loop
                picture(i,j) := ' ';
            end loop;
        end loop;

        lettersUsed := (1..20 => '-');
        wordGuess := (1..20 => '-');
        lettersAlreadyGuessed := (1..26 => ' ');

        for i in 1..12 loop
            picture(i,1) := 'X';
        end loop;

        for j in 1..7 loop
            picture(1,j) := 'X';
        end loop;

        picture(2,7) := 'X';

        errorCount := 0;
        correctGuess := 0;

        reset(gen);
        num := random(gen);
        guessCount := 0;

        word := words(integer(num));
        lengthSpot := wordLengths(integer(num));
    end setup;


-- PROGRAM START
begin
    
    --Populate the values
    setup(picture, wordGuess, lettersAlreadyGuessed, lettersUsed, word, errorCount, correctGuess, guessCount, lengthSpot, wordLengths, words);

    --Intoduction
    put_line("THE GAME OF HANGMAN");
    put_line("---");
    put_line("");

    --Start the GAME
    counter := 1;
    mainLoop: while (counter <= 50) loop
        checker := 0;
        correctGuess := 0;
        put_line("Here are the letters you used:");
        put_line(lettersAlreadyGuessed);

        put_line("Please guess a letter?");
        get(guess);
        skip_line;
        guessCount := guessCount + 1;

        --Check to see if the letter has been guessed already
        for i in 1..guessCount loop

            if (lettersAlreadyGuessed(i) = guess) then
                put_line("You guessed that letter before");
                checker := 1;
                exit when True;
            end if;

            if (lettersAlreadyGuessed(i) = ' ') then
                lettersAlreadyGuessed(i) := guess;
                exit when True;

            end if;
        end loop;

        -- If the letter hasn't been guessed already
        if ( checker = 0) then

            --Check if the letter exists in the word and update the letters used list
            for i in 1..lengthSpot loop
                if ( word(i) = guess) then
                    lettersUsed(i) := guess;
                    correctGuess := correctGuess + 1;
                end if;
            end loop;

            --If the letter guessed was wrong
            if (correctGuess = 0) then
                errorCount := errorCount + 1;

                put_line("Sorry that letter is not in the word");

                if (errorCount = 1) then
                    put_line("First we draw a head.");
                    
                    picture(3,6) := '-';
                    picture(3,7) := '-';
                    picture(3,8) := '-';
                    picture(4,5) := '(';
                    picture(4,6) := '.';
                    picture(4,8) := '.';
                    picture(4,9) := ')';
                    picture(5,6) := '-';
                    picture(5,7) := '-';
                    picture(5,8) := '-';

                elsif (errorCount = 2) then
                    put_line("Now we draw a body.");

                    for i in 6..9 loop
                        picture(i, 7) := 'X';

                    end loop;

                elsif (errorCount = 3) then
                    put_line("Next we draw an arm.");
                    
                    for i in 4..7 loop
                        picture(i, i - 1) := '\';
                    end loop;

                elsif (errorCount = 4) then
                    put_line("This time it's the other arm.");
                    picture(4,11) := '/'; 
                    picture(5,10) := '/'; 
                    picture(6,9) := '/'; 
                    picture(7,8) := '/'; 

                elsif (errorCount = 5) then
                    put_line("Now, let's draw the right leg.");

                    picture(10,6) := '/'; 
                    picture(11,5) := '/';

                elsif (errorCount = 6) then
                    put_line("This time we draw the left leg.");

                    picture(10,8) := '\'; 
                    picture(11,9) := '\';

                elsif (errorCount = 7) then
                    put_line("Now we put up a hand.");
                    picture(3,11) := '\';

                elsif (errorCount = 8) then
                    put_line("Next the other hand.");
                    picture(3,3) := '/';

                elsif (errorCount = 9) then
                    put_line("Now we draw one foot.");
                    picture(12,10) := '\'; 
                    picture(12,11) := '-';

                elsif (errorCount = 10) then
                    put_line("Here's the other foot -- You're hung!!.");
                    
                    picture(12,3) := '-'; 
                    picture(12,4) := '/';
                
                end if;

                for i in 1..12 loop
                    for j in 1..12 loop
                        put(picture(i,j));
                    end loop;

                    put_line("");
                end loop;

                if (errorCount = 10) then
                    put("Sorry, you loose. The word was ");
                    put_line(word);

                    put_line("You missed that one");
                    checker := 0;
                    errorCount := 0; --This way it breaks out and restarts

                end if;

                -- Check if the word has been guessed
                if (errorCount /= 0) then 
                    checker := 0;

                    for i in 1..lengthSpot loop 
                        if ( lettersUsed(i) = '-') then
                            checker := 1;
                            exit when (True);
                        end if;
                    end loop;

                    if (checker = 0) then
                        put_line("You have guessed all the letters!");
                    end if;
                end if;
            
            --If they guessed a letter correctly
            else 

                put_line("Your letter guess was correct");
                checker := 0;

                for i in 1..lengthSpot loop

                    if (lettersUsed(i) = '-') then
                        checker := 1;
                        
                        for i in 1..lengthSpot loop
                            put(lettersUsed(i));
                        end loop;
                        put_line("");

                        put_line("What is your guess for the word: ");
                        get_line(wordGuess, wordLength);

                        --If the user has guessed the word
                        if (wordGuess(1..wordLength) = word(1..wordLength)) then
                            checker := 0;
                        else
                            put_line("Incorrect guess for the word");
                        end if;

                        exit when (True);
                       
                    end if;
                end loop;

                if (checker = 0) then
                    put_line("Congratulations, you guess the word!");
                    put_line("It took you " & integer'image(guessCount) & " guesses");
                end if;
            end if;

            --See if the player wants to play again
            if (checker = 0) then
                getInput(guess);

                if (guess = 'y') then
                    setup(picture, wordGuess, lettersAlreadyGuessed, lettersUsed, word, errorCount, correctGuess, guessCount, lengthSpot, wordLengths, words);
                else
                    exit when (True);
                end if;
            end if;
        end if;
    end loop mainLoop;
    put_line("Thanks for playing!");
end hangman;