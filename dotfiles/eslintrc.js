/* npm install --save-dev \
   eslint \
   eslint-config-airbnb \
   eslint-plugin-cypress \
   eslint-plugin-jest \
   eslint-plugin-react \
   eslint-plugin-react-hooks */
module.exports = {
  env: {
    browser: true,
    es6: true,
    'jest/globals': true,
    'cypress/globals': true,
  },
  extends: 'airbnb',
  plugins: ['react', 'jest', 'cypress'],
};
