import scrapy
import re
class petespider(scrapy.Spider):
    name = "pete"
    allowed_domains = [
    "peteforamerica.com"
    ]
    start_urls = [
    "https://peteforamerica.com/"
        ]

    def parse(self, response):
        for href in response.xpath('//a/@href'):
            url = response.urljoin(href.extract())
            yield scrapy.Request(url, callback=self.parse_dir_contents)


    def parse_dir_contents(self, response):
        fn = re.sub('\\.html', '',response.url) 
        fn2 = re.sub('\\.us|\\.org', '',fn) 
        fn3 = re.sub("https", "", fn2)
        fn4 = re.sub("\W", "", fn3)
        fn5 = re.sub("http|www|com", "", fn4)
        filename =  fn5  + '.html'
        print(filename)
        
        with open(filename, 'wb') as f:
            f.write(response.body)