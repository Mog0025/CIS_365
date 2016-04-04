##############################################################################
# CIS 365 - Project 3
# Dr. Jonathan Leidig
#
# Rock, Paper, Scissors learning AI
# - This program plays games of rock, paper, scissors with the user
# - and keeps track of the player's choices, attempting to develop a
# - strategy that will anticipate the player's behavior
#
# @author Joel Truman & Laura Young
# @version Winter 2016
#############################################################################

import sys
import random

class RockPaperScissors:

    # Instance variables
    def __init__(self):
        print("Welcome Rock, Paper, Scissors bot. I learn from how you play!")
        self.userInput = 0  # User choice
        self.myPlay = 0  # AI player choice
        self.winner = "Parker Lewis"  # Winner of the game
        if len(sys.argv) >= 2:
            self.text_file = str(sys.argv[1])  # text file containing user history
        self.history = []  # array for loading player's past moves
        self.numOnes = 0  # number of times
        self.numTwos = 0  # number of times user picked paper in the past
        self.numThrees = 0  # number of times user picked scissors in the past


    ##############################################################################
    # Prompts user to choose rock, paper, or scissors and displays their choice.
    # If they do not exit, also saves their choice to a text file
    ##############################################################################
    def getInput(self):
        while self.userInput < 1 or self.userInput > 4:
            self.userInput = int(input("Choose 1 for rock, 2 for paper, or 3 for scissors. Type 4 to exit.\n"))
        choiceText = ""
        if self.userInput == 1:
            choiceText = "rock"
        elif self.userInput == 2:
            choiceText = "paper"
        elif self.userInput == 3:
            choiceText = "scissors"
        else:
            choiceText = "to exit. Goodbye!"
        if self.userInput != 4:
            file = open(self.text_file, "a")
            file.write(str(self.userInput) + "\n")
            file.close()
        print("You chose", choiceText)

    ###############################################################
    # Decides AI choice based on historical data
    ###############################################################
    def choosePlay(self):

        # Do not pick a play if user chose to exit
        if self.userInput == 4:
            return

        try:
            # Examine historical data
            self.readHistory()
        except Exception:
            self.userInput = 4
            print("The file you indicated cannot be read. Please try again.")

        # Play randomly on the first turn against a new player
        if len(self.history) == 0:
            self.myPlay = self.randomPlay()

        # Otherwise try each strategy to see which is most likely to win
        else:
            freq = self.tryFrequency()
            stepAhead = self.tryStepAhead()
            rand = self.tryRandom()

            print("Strategy Freq:", freq)
            print("Strategy Step Ahead:", stepAhead)
            print("Strategy Random: ", rand)

            # See which strategy was best and use it
            if freq > max(stepAhead, rand):
                self.myPlay = self.frequencyPlay()
                print("Playing Frequency")
            elif stepAhead > max(freq, rand):
                self.myPlay = self.stepAheadPlay(self.history[-1])
                print("Playing Step Ahead")
            else:
                self.myPlay = self.randomPlay()
                print("Playing Random")

    ##############################################################
    # Reads text file to see how user played in the past
    ##############################################################
    def readHistory(self):
        with open(self.text_file) as file:
            self.history = file.read().splitlines()
            self.numOnes = self.history.count('1')
            self.numTwos = self.history.count('2')
            self.numThrees = self.history.count('3')

    #############################################################
    # Plays against historical data using random plays to
    # determine the win rate of such a strategy
    #############################################################
    def tryRandom(self):
        wins = 0
        numGames = len(self.history)
        for entry in self.history:
            ai = self.randomPlay()
            user = int(entry)
            self.decideWinner(user, ai)
            if self.winner == "me! I'm good at this.":
                wins += 1
        winRate = wins/numGames
        return winRate

    ##############################################################
    # Plays against historical data using frequency plays to
    # determine the win rate of such a strategy
    ##############################################################
    def tryFrequency(self):
        wins = 0
        numGames = len(self.history)
        for entry in self.history:
            ai = self.frequencyPlay()
            user = int(entry)
            self.decideWinner(user, ai)
            if self.winner == "me! I'm good at this.":
                wins += 1
        winRate = wins/numGames
        return winRate

    ##############################################################
    # Plays against historical data using step ahead plays to
    # determine the win rate of such a strategy
    ##############################################################
    def tryStepAhead(self):
        wins = 0
        numGames = len(self.history)
        lastUserPlay = 3  # First play will therefore be scissors
        for entry in self.history:
            ai = self.stepAheadPlay(lastUserPlay)
            user = int(entry)
            lastUserPlay = int(entry)
            self.decideWinner(user, ai)
            if self.winner == "me! I'm good at this.":
                wins += 1
        winRate = wins/numGames
        return winRate

    ##############################################################
    # Prints the AI's choice of play to the screen
    ##############################################################
    def declarePlay(self):
        if self.userInput != 4:
            choiceText = ""
            if self.myPlay == 1:
                choiceText = "rock"
            elif self.myPlay == 2:
                choiceText = "paper"
            else:
                choiceText = "scissors"
            print("I chose", choiceText)

    ##############################################################
    # Compares two different plays to see who won
    ##############################################################
    def decideWinner(self, user, ai):
        if user == 4:
            return
        if user == 1:
            if ai == 1:
                self.winner = "no one. That's unfortunate"
            elif ai == 2:
                self.winner = "me! I'm good at this."
            else:
                self.winner = "you. I'll figure you out!"
        elif user == 2:
            if ai == 2:
                self.winner = "no one. That's unfortunate"
            elif ai == 3:
                self.winner = "me! I'm good at this."
            else:
                self.winner = "you. I'll figure you out!"
        else:
            if ai == 3:
                self.winner = "no one. That's unfortunate"
            elif ai == 1:
                self.winner = "me! I'm good at this."
            else:
                self.winner = "you. I'll figure you out!"

    ##########################################################
    # Prints winner to the screen
    ##########################################################
    def declareWinner(self):
        if self.userInput != 4:
            print("And the winner is...", self.winner)


    ##########################################################
    # Generates a basic random play with no strategy
    ##########################################################
    def randomPlay(self):
        return random.randint(1, 3)

    ##########################################################
    # Generates a play based on looking at how often the
    # user has played each weapon in the past
    ##########################################################
    def frequencyPlay(self):
        percentRock = self.numOnes/len(self.history)
        percentPaper = self.numTwos/len(self.history)
        roll = random.random()  # random float between 0.0 and 1.0
        if roll <= percentRock:
            return 2  # play paper to beat rock
        elif roll <= (percentRock + percentPaper):
            return 3  # play scissors to beat rock
        else:
            return 1  # play rock to beat scissors

    ##########################################################
    # Generates play based on the idea that a player who just
    # lost will often subconsciously pick the throw that
    # beats their last one
    ##########################################################
    def stepAheadPlay(self, lastUserPlay):
        if lastUserPlay == 1:
            return 3  # Play scissors because user likely to play paper
        elif lastUserPlay == 2:
            return 1  # Play rock because user likely to play scissors
        else:
            return 2  # Play paper because user likely to play rock


    ##########################################################
    # Plays Rock, Paper, Scissors until user chooses to exit
    ##########################################################
    def playGame(self):
        if len(sys.argv) < 2:
            print("Usage: provide a path to a text file"
                  " as the first argument.")
            return
        while self.userInput != 4:
            self.choosePlay()
            self.getInput()
            self.declarePlay()
            self.decideWinner(self.userInput, self.myPlay)
            self.declareWinner()
            if self.userInput != 4:
                self.userInput = 0

def main():
    game = RockPaperScissors()
    game.playGame()

main()


