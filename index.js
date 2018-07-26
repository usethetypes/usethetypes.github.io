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

  function makeInfoUrl(episodeId, videoId) {
    return `usethetypes-${episodeId.toString().padStart(3, "0")}.html`;
  }

  const videos = [
    { id: "dN1M8ql1vPQ", episodeId: 0 },
    { id: "TLMjeCN32eM", episodeId: 1 },
    { id: "iNeLpmjowwQ" },
    { id: "cDYn_la-9vg" },
    { id: "hp-uQZ-MujA" },
    { id: "uB7ES5-JhQA" },
    { id: "vcE9BlW74tg" },
    { id: "sP6Iuwd_Nik" },
    { id: "9qiRk5la724" }
  ];

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
      const isEpisode = typeof video.episodeId != "undefined";
      const title = isEpisode ? response.title : `TEST: ${response.title}`;
      anchor
        .attr("title", title)
        .attr("alt", title);
      innerDiv.find("a:first")
        .attr("href", videoUrl)
        .text(title);
      if (isEpisode) {
        innerDiv.find("a:eq(1)")
          .attr("href", makeInfoUrl(video.episodeId, videoId));
      }
      else {
        innerDiv.find("a:eq(1)").toggle(false);
      }
    });
  }
});
