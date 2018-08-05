.PHONY: default
default: preview

.PHONY: init
init:
	rbenv install 2.5.1
	gem install bundler
	bundle install

.PHONY: preview
preview:
	bundle exec rake preview

.PHONY: test
test:
	bundle exec rake test
