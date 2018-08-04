$(() => {
  function makeYouTubeUrl(video) {
    return `https://youtu.be/${video.youTubeId}`;
  }

  function makeImageUrl(video) {
    return `https://img.youtube.com/vi/${video.youTubeId}/maxresdefault.jpg`;
  }

  function makeNoEmbedUrl(video) {
    return `https://noembed.com/embed?url=https://www.youtube.com/watch?v=${video.youTubeId}`;
  }

  $.get({
    url: "/videos.json",
    dataType: "json"
  }).then(videos => {
    const container = $("#carousel .carousel-inner");
    const divTemplate = $("#carousel-image-template");

    for (const video of videos) {
      const youTubeUrl = makeYouTubeUrl(video);

      const div = $(divTemplate.html());

      const a = div.find("> :first-child");
      a.attr("href", youTubeUrl);

      const img = a.find("> :first-child");
      img.attr("src", makeImageUrl(video));

      container.append(div);

      $.get({
        url: makeNoEmbedUrl(video),
        dataType: "json"
      }).then(response => {
        const innerDiv = div.find("div:first");
        a
          .attr("title", response.title)
          .attr("alt", response.title);
        innerDiv.find("a:first")
          .attr("href", youTubeUrl)
          .text(response.title);
        innerDiv.find("a:eq(1)")
          .attr("href", video.href);
      });
    }

    container.find("div:first").addClass("active");
  });
});
