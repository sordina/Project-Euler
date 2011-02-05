require 'rubygems'
require 'nokogiri'
require 'open-uri'

(1..300).each do |n|
	puts "---- Problem #{n}"

	page = open "http://projecteuler.net/index.php?section=problems&id=#{n}"
	contents = page.read
	problem = Nokogiri::HTML.parse(contents).css(".problem_content")
	question = problem.text

	File.open "Problems/#{n.to_s.rjust 3, '0'}_problem.txt", 'w' do |f|
		f.puts question
	end
end
