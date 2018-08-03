$(() => {
  function makeVideoUrl(videoId) {
    return `https://youtu.be/${videoId}`;
  }

  function makeImageUrl(videoId) {
    return `https://img.youtube.com/vi/${videoId}/maxresdefault.jpg`;
  }

  function makeNoEmbedUrl(videoId) {
    return `https://noembed.com/embed?url=https://www.youtube.com/watch?v=${videoId}`;
  }

  function renderCarousel(videos) {
    const divContainer = $("#carousel .carousel-inner");
    const divTemplate = $("#carousel-template-image");

    for (let i = 0; i < videos.length; ++i) {
      const video = videos[i];
      const videoId = video.id;
      const videoUrl = makeVideoUrl(videoId);

      const div = $(divTemplate.html());
      div.attr("id", `video-${videoId}`);

      const anchor = div.find("> :first-child");
      anchor.attr("href", videoUrl);

      const image = anchor.find("> :first-child");
      image.attr("src", makeImageUrl(videoId));

      if (i == 0) {
        div.addClass("active");
      }

      divContainer.append(div);

      $.get({
        url: makeNoEmbedUrl(videoId),
        dataType: "json"
      }).then(response => {
        const innerDiv = div.find("div:first");
        anchor
          .attr("title", response.title)
          .attr("alt", response.title);
        innerDiv.find("a:first")
          .attr("href", videoUrl)
          .text(response.title);
        innerDiv.find("a:eq(1)")
          .attr("href", video.href);
      });
    }
  }

  $.get({
    url: "/videos.json",
    dataType: "json"
  }).then(renderCarousel);
});
