const axios = require('axios');
require('dotenv').config();

const { NAVER_CLIENT_ID, NAVER_CLIENT_SECRET } = process.env;

module.exports = function getBookData(title) {
  return axios({
    method: 'get',
    url: 'https://openapi.naver.com/v1/search/book_adv',
    params: {
      d_titl: title,
      display: 1
    },
    headers: {
      'X-Naver-Client-Id': NAVER_CLIENT_ID,
      'X-Naver-Client-Secret': NAVER_CLIENT_SECRET
    }
  });
};
