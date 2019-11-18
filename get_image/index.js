const Sequlize = require('sequelize');
const createBulkBookData = require('./utils');
require('dotenv').config();

const { DB_NAME, DB_USERNAME, DB_PASSWORD, DB_HOST } = process.env;

const sequelize = new Sequlize(DB_NAME, DB_USERNAME, DB_PASSWORD, {
  host: DB_HOST,
  dialect: 'mysql'
});

const BookList = sequelize.define(
  'book_list',
  {
    id: {
      type: Sequlize.INTEGER(11),
      allowNull: false,
      primaryKey: true,
      autoIncrement: true
    },
    bookname: Sequlize.STRING(512),
    imageUrl: Sequlize.STRING(256),
    author: Sequlize.STRING(256)
  },
  {
    tableName: 'book_list',
    timestamps: false
  }
);

sequelize
  .query(
    'select bookname, count(*) from borrow_list group by bookname having count(*)>100'
  )
  .then(datas => {
    createBulkBookData(datas[0]).then(bulkData => {
      BookList.bulkCreate(bulkData);
    });
  })
  .catch(err => console.error(err));
