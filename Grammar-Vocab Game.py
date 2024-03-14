from PyDictionary import PyDictionary
import urllib.request
import random


#Translate link of random words into usable text
def VocabGame():
    #Generate Dictionary and Link to recieve possible words
    dict = PyDictionary()
    word_site = "https://www.mit.edu/~ecprice/wordlist.10000"

    #Initialize Variables
    response = urllib.request.urlopen(word_site)
    txt = response.read().decode()
    WordChoice = txt.splitlines()
    #isContinuing = True
    FinalScore = 0
    j = 0
    print("---------------------------------------------------------------------")
    print("The Vocab Game is starting! We will ask you the definition of 10 words, try to get all of them right!")
    #Start loop for running Vocab Game
    while j < 10:
        j += 1
        #Create random word with definition
        CurrentWord = random.choice(WordChoice)
        CurrentWordNumber = random.randint(1, 4)
        Meaning = dict.meaning(CurrentWord, disable_errors=True)

        #Display meaning and get answer
        if Meaning is not None:
            print("---------------------------------------------------------------------")
            print("Question %d---" % (j))
            print("The definition of your word is:", Meaning)
            print("\n")
            print("Options are:")
            i = 0
            #Show potential answers
            Options = []
            while i<5:
                i += 1
                if i == CurrentWordNumber:
                    print(i, '--', CurrentWord)
                    Options.append(CurrentWord)
                else:
                    RandomWord = random.choice(WordChoice)
                    print(i, '--', RandomWord)
                    Options.append(RandomWord)
            # Get user Answer
            answer = input("Enter correct answer: ")

            #If right
            if int(answer) == CurrentWordNumber:
                FinalScore += 1
                print("Correct!")
            #if wrong print right answer
            else:
                print("No the corrent answer was:", CurrentWord)
                print("\n")
                selection = int(answer)
                selectedWord = Options[selection - 1]
                selectedMeaning = dict.meaning(selectedWord, disable_errors=True)
                print("You chose:", selectedWord)
                if selectedMeaning is not None:
                    print("The meaning of %s is %s" % (selectedWord, selectedMeaning))
                else:
                    print("That word does not have an availabe definition.\n")

                print("---------------------------------------------------------------------")

        # Does want to continue playing

        #willPlay = input("Do you want to continue playing Vocab Game, enter 'Yes' or 'No'?: ")
        #if willPlay == 'No':
        #    isContinuing = False
        #elif willPlay == 'Yes':
        #    print("\n")
        #elif willPlay != 'Yes' or willPlay != 'No':
        #    print("Error on input! Coninuing game, next time please enter 'Yes' or 'No'.\n")
    print("Vocab game is finsihed!")
    print("You got %d out of 10 correct!" % (FinalScore))
    if FinalScore in [8, 9, 10]:
        print("Well Done!")
        print("---------------------------------------------------------------------")
    if FinalScore in [6, 7]:
        print("Good job! We recommend practicing one or two more times!")
        print("---------------------------------------------------------------------")
    if FinalScore in [1, 2, 3, 4, 5]:
        print("Good effort! We highly recommend continually replaying this game.")
        print("---------------------------------------------------------------------")


def GrammarGame():
    print("---------------------------------------------------------------------")
    print("Welcome to the Grammar Game!")
    print("For the following sentences, select the answer that correctly fills in the blank")
    questions = ["I am going to ___ house.", 
                 "I am ___ introvert.", 
                 "___ and ___ are going to the bank.",
                 "The team is playing ____.",
                 "To see well, the lights in this room must be adjusted.",
                 "The suspects were interviewed right after the crime was commited by the detectives.",
                 "Neither of my classmates ___ taking the trip this year.",
                 "When I did the experiment, I had to ____ _____ each of the measurments.",
                 "My parents met at college in the ____.", 
                 "Unfortunately, John has ___ friends in New York."]
    responses = ["1. Their\n2. They're\n3. There\n", 
                 "1. a\n2. an\n", 
                 "1. Josh, I\n2. Me, Josh\n", 
                 "1. Good\n2. Well\n",
                 "Is this sentence correct?\n1. Yes\n2. No\n",
                 "Pick the correct option.\n1. This sentence is correct\n2. The verb in this sentence needs to be in a different form\n3. The modifer is misplaced",
                 "1. are\n2. is\n",
                 "1. thoroughly document\n2. document thoroughly\n",
                 "1. '80s\n2. 80s\n3. 80's\n",
                 "1. few\n2. a few\n3. a lot\n4. many\n"]
    correctResponse = [1, 2, 1, 2, 2, 3, 2, 2, 1, 1]


    print("---------------------------------------------------------------------")
    numberOfQ = input("How many questions would you like to answer? Pick a number 1-10: ")
    numQ = int(numberOfQ)
    j = 0
    asked = []
    score = 0
    while j<numQ:
        choice = random.randint(0, 9)
        if choice not in asked:
            j += 1
            asked.append(choice)
            print(questions[choice])
            print(responses[choice])
            
            
            answer = input("Pick the correct response: ")
            if int(answer) == correctResponse[choice]:
                print("Correct!")
                score += 1
            else:
                print("Sorry that is incorrect, the right choice was:", correctResponse[choice])
            print("---------------------------------------------------------------------")
    print("Game is over! Youre score is: %d/%d" % (score, numQ))
    if numQ == 10:
        if score in [8, 9, 10]:
            print("Well Done!")
            print("---------------------------------------------------------------------")
        if score in [6, 7]:
            print("Good job! We recommend practicing one or two more times!")
            print("---------------------------------------------------------------------")
        if score in [1, 2, 3, 4, 5]:
            print("Good effort! We highly recommend continually replaying this game.")
            print("---------------------------------------------------------------------")


def main():
    print("---------------------------------------------------------------------")
    print("Welcome to Our Vocab/Grammar Game!")
    print("---------------------------------------------------------------------")
    playing = True
    while playing == True:
        gameChoice = input("SYSTEM: Would you like to test 'Vocab', 'Grammar', or would you like to 'Quit'?: ")
        if gameChoice == 'Vocab':
            VocabGame()
            switch = input("SYSTEM: Would you like to continue? ('Yes' or 'No'): ")
            if switch == 'No':
                playing = False
                print("SYSTEM: Thanks for playing!")
            if switch == 'Yes':
                playing = True

        if gameChoice == 'Grammar':
            GrammarGame()
            switch = input("SYSTEM: Would you like to continue? ('Yes' or 'No'): ")
            if switch == 'No':
                playing = False
                print("SYSTEM: Thanks for playing!")
            if switch == 'Yes':
                playing = True

        if gameChoice == 'Quit':
            playing = False
            print("SYSTEM: Thanks for playing!")

        elif gameChoice not in ['Quit', 'Grammar', 'Vocab']:
            print("Incorrect input, please try again.")

main()