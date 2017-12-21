download_setup_eigen()
{
  mkdir eigen
  curl -L "http://bitbucket.org/eigen/eigen/get/3.3.4.tar.bz2" | tar xv -C eigen --strip-components=1
}

if [ ! -d "eigen/" ]; then
  download_setup_eigen
  echo "Finished installing eigen!"
else
  read -p "The eigen folder already exists! Do you want to remove the folder and reinstall? " yn
  case $yn in
    [Yy]* ) rm -rf eigen; download_setup_eigen;;
    [Nn]* ) exit;;
    * ) echo "Please enter Y or N";;
  esac

fi
