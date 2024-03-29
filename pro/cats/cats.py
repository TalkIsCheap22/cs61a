"""Typing test implementation"""

from utils import *
from ucb import main, interact, trace
from datetime import datetime


###########
# Phase 1 #
###########


def choose(paragraphs, select, k):
    """Return the Kth paragraph from PARAGRAPHS for which SELECT called on the
    paragraph returns true. If there are fewer than K such paragraphs, return
    the empty string.
    """
    # BEGIN PROBLEM 1
    "*** YOUR CODE HERE ***"
    count=0
    for paragraph in paragraphs:
        if select(paragraph)==True:
            count+=1
        if count>k:
            return paragraph
    return ""
    # END PROBLEM 1


def about(topic):
    """Return a select function that returns whether a paragraph contains one
    of the words in TOPIC.

    >>> about_dogs = about(['dog', 'dogs', 'pup', 'puppy'])
    >>> choose(['Cute Dog!', 'That is a cat.', 'Nice pup!'], about_dogs, 0)
    'Cute Dog!'
    >>> choose(['Cute Dog!', 'That is a cat.', 'Nice pup.'], about_dogs, 1)
    'Nice pup.'
    """
    assert all([lower(x) == x for x in topic]), 'topics should be lowercase.'
    # BEGIN PROBLEM 2
    "*** YOUR CODE HERE ***"
    def get_words(paragraph):
        lp=len(paragraph)
        start_index,words,word=0,[],''
        for i in range(lp):
            stuff=paragraph[i]
            if stuff<='z' and stuff>='a':
                word+=stuff
                if i==lp-1:
                    words.append(word)
            elif stuff==' ' or i==lp-1:
                words.append(word)
                word=''
        return words
    def judge(paragraph):
        lower_paragraph=paragraph.lower()
        words=get_words(lower_paragraph)
        for word in topic:
            if word in words:
                return True
        return False
    return judge
    # END PROBLEM 2


def accuracy(typed, reference):
    """Return the accuracy (percentage of words typed correctly) of TYPED
    when compared to the prefix of REFERENCE that was typed.

    >>> accuracy('Cute Dog!', 'Cute Dog.')
    50.0
    >>> accuracy('A Cute Dog!', 'Cute Dog.')
    0.0
    >>> accuracy('cute Dog.', 'Cute Dog.')
    50.0
    >>> accuracy('Cute Dog. I say!', 'Cute Dog.')
    50.0
    >>> accuracy('Cute', 'Cute Dog.')
    100.0
    >>> accuracy('', 'Cute Dog.')
    0.0
    """
    typed_words = split(typed)
    reference_words = split(reference)
    # BEGIN PROBLEM 3
    "*** YOUR CODE HERE ***"
    lt,lr,correspond_count=len(typed_words),len(reference_words),0
    for i in range(min(lt,lr)):
        if typed_words[i]==reference_words[i]:
            correspond_count+=1
    return 0.0 if lt==0 else correspond_count/lt*100
    # END PROBLEM 3


def wpm(typed, elapsed):
    """Return the words-per-minute (WPM) of the TYPED string."""
    assert elapsed > 0, 'Elapsed time must be positive'
    # BEGIN PROBLEM 4
    "*** YOUR CODE HERE ***"
    return len(typed)/elapsed*12
    # END PROBLEM 4


def autocorrect(user_word, valid_words, diff_function, limit):
    """Returns the element of VALID_WORDS that has the smallest difference
    from USER_WORD. Instead returns USER_WORD if that difference is greater
    than LIMIT.
    """
    # BEGIN PROBLEM 5
    "*** YOUR CODE HERE ***"
    min_diff,closest_word=float('inf'),''
    for valid_word in valid_words:
        if valid_word==user_word:
            return user_word
        this_diff=diff_function(user_word,valid_word,limit)
        if this_diff<min_diff:
            min_diff=this_diff
            closest_word=valid_word
    if min_diff>limit:
        return user_word
    return closest_word
    # END PROBLEM 5


def shifty_shifts(start, goal, limit):
    """A diff function for autocorrect that determines how many letters
    in START need to be substituted to create GOAL, then adds the difference in
    their lengths.
    """
    # BEGIN PROBLEM 6
    ls,lg=len(start),len(goal)
    lmin,lmax=min(ls,lg),max(ls,lg)
    start,goal=start[:lmin],goal[:lmin]
    count=lmax-lmin
    def total_diff(s1,s2):
        nonlocal count
        if count>limit:
            return
        if len(s1)==1 and s1!=s2:
            count+=1
        elif s1!=s2:
            count+=0 if s1[0]==s2[0] else 1
            total_diff(s1[1:],s2[1:])
    total_diff(start,goal)
    return count
    
    # END PROBLEM 6


def meowstake_matches(start, goal, limit):
    """A diff function that computes the edit distance from START to GOAL."""
    ls,lg=len(start),len(goal)
    if limit<0:
        return 0
    elif ls*lg==0:
        return ls+lg
    elif start[0]==goal[0]:
        return meowstake_matches(start[1:],goal[1:],limit)
    add_diff=meowstake_matches(start,goal[1:],limit-1)
    delete_diff=meowstake_matches(start[1:],goal,limit-1)
    change_diff=meowstake_matches(start[1:],goal[1:],limit-1)
    return min(add_diff,delete_diff,change_diff)+1


def final_diff(start, goal, limit):
    """A diff function. If you implement this function, it will be used."""
    assert False, 'Remove this line to use your final_diff function'


###########
# Phase 3 #
###########


def report_progress(typed, prompt, id, send):
    """Send a report of your id and progress so far to the multiplayer server."""
    # BEGIN PROBLEM 8
    "*** YOUR CODE HERE ***"
    lt,lp,count=len(typed),len(prompt),0
    for i in range(lt):
        if typed[i]==prompt[i]:
            count+=1
        else:
            break
    progress=count/lp
    print_dict={'id':id,'progress':progress}
    send(print_dict)
    return progress
    # END PROBLEM 8


def fastest_words_report(times_per_player, words):
    """Return a text description of the fastest words typed by each player."""
    game = time_per_word(times_per_player, words)
    fastest = fastest_words(game)
    report = ''
    for i in range(len(fastest)):
        words = ','.join(fastest[i])
        report += 'Player {} typed these fastest: {}\n'.format(i + 1, words)
    return report


def time_per_word(times_per_player, words):
    """Given timing data, return a game data abstraction, which contains a list
    of words and the amount of time each player took to type each word.

    Arguments:
        times_per_player: A list of lists of timestamps including the time
                          the player started typing, followed by the time
                          the player finished typing each word.
        words: a list of words, in the order they are typed.
    """
    # BEGIN PROBLEM 9
    "*** YOUR CODE HERE ***"
    lt=len(times_per_player)
    for i in range(lt):
        stamps=times_per_player[i]
        for j in range(len(stamps)-1):
            stamps[j]=stamps[j+1]-stamps[j]
        stamps.pop()
    return game(words,times_per_player)
            
    # END PROBLEM 9


def fastest_words(game):
    """Return a list of lists of which words each player typed fastest.

    Arguments:
        game: a game data abstraction as returned by time_per_word.
    Returns:
        a list of lists containing which words each player typed fastest
    """
    players = range(len(all_times(game)))  # An index for each player
    words = range(len(all_words(game)))    # An index for each word
    # BEGIN PROBLEM 10
    "*** YOUR CODE HERE ***"
    fastest_list=[]
    for i in range(len(players)):
        fastest_list.append([])
    for word in words:
        fastest,fastest_player=float('inf'),0
        for player in players:
            temp=time(game,player,word)
            if fastest>temp:
                fastest=temp
                fastest_player=player
        fastest_list[fastest_player].append(word_at(game,word))
    return fastest_list
    # END PROBLEM 10


def game(words, times):
    """A data abstraction containing all words typed and their times."""
    assert all([type(w) == str for w in words]), 'words should be a list of strings'
    assert all([type(t) == list for t in times]), 'times should be a list of lists'
    assert all([isinstance(i, (int, float)) for t in times for i in t]), 'times lists should contain numbers'
    assert all([len(t) == len(words) for t in times]), 'There should be one word per time.'
    return [words, times]


def word_at(game, word_index):
    """A selector function that gets the word with index word_index"""
    assert 0 <= word_index < len(game[0]), "word_index out of range of words"
    return game[0][word_index]


def all_words(game):
    """A selector function for all the words in the game"""
    return game[0]


def all_times(game):
    """A selector function for all typing times for all players"""
    return game[1]


def time(game, player_num, word_index):
    """A selector function for the time it took player_num to type the word at word_index"""
    assert word_index < len(game[0]), "word_index out of range of words"
    assert player_num < len(game[1]), "player_num out of range of players"
    return game[1][player_num][word_index]


def game_string(game):
    """A helper function that takes in a game object and returns a string representation of it"""
    return "game(%s, %s)" % (game[0], game[1])

enable_multiplayer = False  # Change to True when you

##########################
# Extra Credit #
##########################

key_distance = get_key_distances()
def key_distance_diff(start, goal, limit):
    """ A diff function that takes into account the distances between keys when
    computing the difference score."""

    start = start.lower() #converts the string to lowercase
    goal = goal.lower() #converts the string to lowercase

    # BEGIN PROBLEM EC1
    "*** YOUR CODE HERE ***"
    ls,lg=len(start),len(goal)
    if limit<0:
        return float('inf')
    elif ls*lg==0:
        return ls+lg
    elif start[0]==goal[0]:
        return key_distance_diff(start[1:],goal[1:],limit)
    add_diff=key_distance_diff(start,goal[1:],limit-1)+1
    delete_diff=key_distance_diff(start[1:],goal,limit-1)+1
    key_dist=key_distance[(start[0],goal[0])]
    change_diff=key_distance_diff(start[1:],goal[1:],limit-1)+key_dist
    return min(add_diff,delete_diff,change_diff)
    # END PROBLEM EC1

def memo(f):
    """A memoization function as seen in John Denero's lecture on Growth"""

    cache = {}
    def memoized(*args):
        if args not in cache:
            cache[args] = f(*args)
        return cache[args]
    return memoized

key_distance_diff = count(key_distance_diff)

cache={}
def faster_autocorrect(user_word, valid_words, diff_function, limit):
    """A memoized version of the autocorrect function implemented above."""

    # BEGIN PROBLEM EC2
    "*** YOUR CODE HERE ***"
    if user_word in valid_words:
        return user_word
    min_diff,min_word=float('inf'),''
    for valid_word in valid_words:
        if [user_word,valid_word] not in cache:
            cache[(user_word,valid_word)]=cache[(valid_word,user_word)]=diff_function(user_word,valid_word,limit)
        this_diff=cache[(user_word,valid_word)] 
        if this_diff<min_diff:
            min_diff=this_diff
            min_word=valid_word
    if min_diff>limit:
        return user_word
    return min_word
    # END PROBLEM EC2


##########################
# Command Line Interface #
##########################


def run_typing_test(topics):
    """Measure typing speed and accuracy on the command line."""
    paragraphs = lines_from_file('data/sample_paragraphs.txt')
    select = lambda p: True
    if topics:
        select = about(topics)
    i = 0
    while True:
        reference = choose(paragraphs, select, i)
        if not reference:
            print('No more paragraphs about', topics, 'are available.')
            return
        print('Type the following paragraph and then press enter/return.')
        print('If you only type part of it, you will be scored only on that part.\n')
        print(reference)
        print()

        start = datetime.now()
        typed = input()
        if not typed:
            print('Goodbye.')
            return
        print()

        elapsed = (datetime.now() - start).total_seconds()
        print("Nice work!")
        print('Words per minute:', wpm(typed, elapsed))
        print('Accuracy:        ', accuracy(typed, reference))

        print('\nPress enter/return for the next paragraph or type q to quit.')
        if input().strip() == 'q':
            return
        i += 1


@main
def run(*args):
    """Read in the command-line argument and calls corresponding functions."""
    import argparse
    parser = argparse.ArgumentParser(description="Typing Test")
    parser.add_argument('topic', help="Topic word", nargs='*')
    parser.add_argument('-t', help="Run typing test", action='store_true')

    args = parser.parse_args()
    if args.t:
        run_typing_test(args.topic)