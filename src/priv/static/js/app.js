function View (selector) {
    this._element = $(selector);

    this._createTable();
}

View.prototype._createCommentBox = function () {
    var source   = $("#comment-template").html();
    var template = Handlebars.compile(source);
    return $(template());
};

View.prototype._handleCommentClick = function (event) {
    console.log('handleCommentClick this', this, $(event.target));
    var parentRow = $(event.target).parents('.document-line');
    console.log('parentRow', parentRow);

    var commentRows = parentRow.next('.comment-row');
    var isShowingCommentInput = commentRows.length ? true : false;
    if (isShowingCommentInput) {
        var commentRow = commentRows.first();
        commentRow.remove();
    } else {
        var commentBox = this._createCommentBox();
        commentBox.insertAfter(parentRow);

        $('#save-button').on('click', this._handleSaveComment);
        $('#cancel-button').on('click', this._handleCancelComment);
    }
};

View.prototype._createRow = function (lineNum, contents) {
    var source   = $("#row-template").html();
    var template = Handlebars.compile(source);
    var row = $(template({
        lineNum: lineNum,
        contents: contents
    }));
    var commentLinks = row.find('.comment-link');
    commentLinks.on('click', _.bind(this._handleCommentClick, this));
    return $(row);
};

View.prototype._handleSaveComment = function () {
    console.log('_handleSaveComment', this);
    var parentRow = $(this).parents('.comment-row').first();
    var input = parentRow.find('#comment-input');
    console.log('input', input.val());
};

View.prototype._handleCancelComment = function () {
    console.log('_handleCancelComment', this);
};

View.prototype._createTable = function () {
    var table = $('<table>');
    var i;
    var rowCount = 20;
    for (i = 0; i < rowCount; i++) {
        var row = this._createRow(i, 'sup'+i);
        row.appendTo(table);
    }
    table.appendTo(this._element);

};

$(function () {
    console.log($('#view'));
    var view = new View('#view');
});
