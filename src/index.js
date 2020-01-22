import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import AWSLibrary from './aws-library';

const library = new AWSLibrary();

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    startingWidth: window.innerWidth,
    startingHeight: window.innerHeight
  }
});

app.ports.imageLoaded.subscribe((image) => {
  library.processImage(image.name, image.content)
    .then(
      (faceResult) => {
        //Do better 
        if (faceResult.error) {
          console.log("error");
          console.log(faceResult);
        }
        else {
          console.log("No error");
          console.log(faceResult);
          app.ports.faceDataReceived.send(faceResult);
        }
      });

});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
