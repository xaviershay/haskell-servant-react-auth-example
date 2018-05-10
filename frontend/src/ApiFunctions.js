class Api {
  constructor(jwt) { this.jwt = jwt }

getEmail() {
  return fetch('http://localhost:8000/email', { bogus: true
    , headers: { "Authorization": "Bearer " + this.jwt }
  });
}

getUnprotected() {
  return fetch('http://localhost:8000/unprotected', { bogus: true
  });
}

}
export { Api as default }