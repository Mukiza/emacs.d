# Install Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

#Java is required by a few apps so it gets here first.
brew tap caskroom/versions
brew cask install java8

#brew formulas
brew install git ispell openconnect go elixir sbt scala idris tig scala apache-spark tmux vim jenv kafka sbt terraform reattach-to-user-namespace ansible tree

#brew casks
brew cask install android-studio
brew cask install android-sdk
brew postinstall fontconfig
brew cask install tunnelblick
brew cask install gpg-suite
brew cask install google-chrome
brew cask install docker
brew cask install spectacle
brew cask install 1password
brew cask install intellij-idea
brew cask install visual-studio-code
brew cask install dropbox
brew cask install awscli

#Install RVM
gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 7D2BAF1CF37B13E2069D6956105BD0E739499BDB
curl -sSL https://get.rvm.io | bash -s stable --ruby --auto-dotfiles

#Oh my Zsh shell
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

#installing nvm
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.8/install.sh | bash

echo 'export NVM_DIR="$HOME/.nvm" [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm' >> ~/.zshrc

#increase the speed of the cursor
defaults write NSGlobalDomain KeyRepeat -int 0

echo 'export PATH="$HOME/.jenv/bin:$PATH"' >> ~/.zshrc
echo 'eval "$(jenv init -)"' >> ~/.zshr
