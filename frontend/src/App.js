/* global gapi */

import React, { Component } from 'react';
import './App.css';
import Api from './ApiFunctions.js';

var GoogleAuth;
var SCOPE = 'email https://www.googleapis.com/auth/calendar'

class App extends Component {
  constructor() {
    super();
    this.state = {
      quotes: []
    }
  }

  login() {
    GoogleAuth.signIn();
  }

  logout() {
    GoogleAuth.signOut();
    this.setState({
      "user": null,
      "email": null
    })
  }

  initClient() {
    let me = this;

    // Initialize the gapi.client object, which app uses to make API requests.
    // Get API key and client ID from API Console.
    // 'scope' field specifies space-delimited list of access scopes.
    window.gapi.client.init({
        'apiKey': 'API_KEY',
        'discoveryDocs': [],
        'clientId': 'CLIENT_ID',
        'scope': SCOPE
    }).then(function () {
      GoogleAuth = window.gapi.auth2.getAuthInstance();

      // Listen for sign-in state changes.
      GoogleAuth.isSignedIn.listen(me.handleSignIn.bind(me));

      me.handleSignIn();
    });
  }

  handleSignIn() {
    let me = this;
    let user = GoogleAuth.currentUser.get();
    let isAuthorized = user.hasGrantedScopes(SCOPE);

    me.setState({"user": user})

    if (isAuthorized) {
      var token = user.getAuthResponse().id_token;

      window.api = new Api(token);

      window.api.getEmail()
        .then((resp) => {
          resp.json().then((data) => {
            me.setState({"email": data})
          })
        });
    }
  }

  loggedIn() {
    return this.state.user && this.state.user.hasGrantedScopes(SCOPE)
  }

  componentDidMount() {
    let me = this;
    const script = document.createElement("script");
    script.src = "https://apis.google.com/js/api.js";

    script.onload = () => {
      gapi.load('client', () => me.initClient.bind(me)())
    }
    document.body.appendChild(script)
  }

  render() {
    return (
      <div className="App">
        {this.loggedIn() ?
          <a href='#logout' onClick={this.logout.bind(this)}>Logout</a> :
          <a href='#login' onClick={this.login.bind(this)}>Login</a>
        }

        {this.state.email ?
          <p>{this.state.email}</p> :
          <p>Account not yet loaded from API</p>
        }
      </div>
    );
  }
}

export default App;
