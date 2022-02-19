package example.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import example.model.Channel;
import example.repo.JpaChannelRepository;

import java.util.List;

@Service
@RequiredArgsConstructor
public class ChannelService {

    private final JpaChannelRepository channelRepository;

    public List<Channel> findAll() {
        return channelRepository.findAll();
    }

    public Channel update(Channel channel) {
        return channelRepository.save(channel);
    }

    public void delete(String id) {
        channelRepository.deleteById(id);
    }
}
