# -*- coding: utf-8 -*-
from selenium import webdriver
from bs4 import BeautifulSoup

#open browser
browser = webdriver.Chrome()

url = "https://twitter.com/login"
browser.get(url)

username = browser.find_element_by_class_name("js-username-field")
password = browser.find_element_by_class_name("js-password-field")

username.send_keys("mkrupenk@stanford.edu")
#password.send_keys(ur password here)

browser.find_element_by_xpath("//button[@class='submit EdgeButton EdgeButton--primary EdgeButtom--medium']").click()

innerHTML = browser.execute_script("return document.body.innerHTML")

#parse with BS4
soup = BeautifulSoup(innerHTML, 'html.parser')
                
content = soup.find_all("p","TweetTextSize js-tweet-text tweet-text")