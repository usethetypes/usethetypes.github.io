$(() => {
    $.get({
    url: `/videos.json?${TAG}`,
    dataType: "json"
  }).then(videos => {
    const ul = $("#videos");
    const liTemplate = $("#list-item-template");
    for (const video of videos) {
      const li = $(liTemplate.html());
      li.find("> :first-child")
        .attr("href", video.href)
        .text(video.title);
      ul.append(li);
    }
  });
});
