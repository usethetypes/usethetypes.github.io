$(() => {
  function makeVideoUrl(videoId) {
    return `https://youtu.be/${videoId}`;
  }

  function makeImageUrl(videoId) {
    return `https://img.youtube.com/vi/${videoId}/0.jpg`;
  }

  const videoIds = [
    "iNeLpmjowwQ",
    "cDYn_la-9vg",
    "hp-uQZ-MujA",
    "uB7ES5-JhQA",
    "vcE9BlW74tg",
    "sP6Iuwd_Nik",
    "9qiRk5la724"
  ];

  const divContainer = $("#carousel .carousel-inner");
  const divTemplate = $("#carousel-template-image");

  for (let i = 0; i < videoIds.length; ++i) {
    const div = $(divTemplate.html());

    const anchor = div.find("> :first-child");
    anchor.attr("href", makeVideoUrl(videoIds[i]));

    const image = anchor.find("> :first-child");
    image.attr("src", makeImageUrl(videoIds[i]));

    if (i == 0) {
      div.addClass("active");
    }

    divContainer.append(div);
  }
});
