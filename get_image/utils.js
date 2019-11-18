const getBookData = require('./book');

module.exports = async function createBulkBookData(rawDatas) {
  let tmp_arr = [];

  for (const item of rawDatas) {
    const bookData = await setBookData(item.bookname);

    tmp_arr.push(bookData);
  }

  return tmp_arr;
};

async function setBookData(bookname) {
  const {
    data: { items }
  } = await getBookData(bookname);

  if (!items.length) return { bookname };

  return {
    bookname,
    author: items[0].author,
    image: items[0].image
  };
}
