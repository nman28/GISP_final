from selenium import webdriver
import time

def scrapePlaybyPlay(startId, endId):
	"""startId and endID are integers, corresponding to 
	the last three digits of that string 0021500492.

	so gameID in the above case would be 492, the integer

	For the game 0021500001, the game id would be 1 (not 001)"""
	driver = webdriver.Chrome()

	for i in range(startId, endId):
		stringToSend = str(i)

		while len(stringToSend) < 3:
			stringToSend = '0' + stringToSend

		scrapeGame(driver, stringToSend)
		time.sleep(1)

	driver.quit()


def scrapeGame(driver, gameId):
	"""Scrapes play by play data in JSON format from NBA.com.
	gameID is the last three digits of that annoying string 0021500492 or whatever.

	gameID is in the format of a string!!!

	So to scrape for the game 0021500492, gameid is the string "492"

	We will scrape all games from 0021500001 to 0021500663. This
	should cover all the games in the Github, and then some. For instance,
	0021500640 is a game between the Cavs and the Clippers on Jan. 21, 2016,
	which we don't have SportVU data for."""

	urlStart = "https://stats.nba.com/stats/playbyplayv2?EndPeriod=10&EndRange=55800&GameID=0021500"
	urlEnd = "&RangeType=2&StartPeriod=1&StartRange=0"

	url = urlStart + gameId + urlEnd

	driver.get(url)

	body = driver.find_element_by_tag_name('body')
	text = body.text

	fileName = "0021500" + gameId + "pbp.json"
	outFile = open("C:/Users/liaos/OneDrive/Documents/Brown/Senior Year/GISP_final/data/PBP_data/" + fileName, 'w')
	outFile.write(text)
	outFile.close()

scrapePlaybyPlay(1, 664)


